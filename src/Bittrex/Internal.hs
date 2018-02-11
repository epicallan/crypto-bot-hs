module Bittrex.Internal (
        callAPI
    ,   defOpts) where

import           Bittrex.Types
import           Control.Exception.Safe (catchAsync)
import           Data.Aeson
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy   as LBS
import           Data.List
import           Data.String            (String)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Lens.Micro
import           Lens.Micro.Aeson
import           Network.HTTP.Client
import qualified Network.Wreq           as Nw
import           Protolude              hiding (get)

type URL = String

defOpts :: APIOpts
defOpts = APIOpts PublicAPI [] "v1.1" mempty

makeBaseUrl :: APIOpts -> String
makeBaseUrl APIOpts{..} =
    [ "https://bittrex.com/api"
    , Text.unpack apiOptsVersion
    , show apiOptsAPIType
    , Text.unpack apiOptsPath
    ] & intercalate "/"

urlWithParams :: APIOpts -> URL -> URL
urlWithParams APIOpts{..} url =
    [url, "?", mconcat (Text.unpack <$> params')] & mconcat & init
    where
        go (k, v) = k <> "=" <> v <> "&"
        params':: [Text]
        params' = go <$> apiOptsQueryParams

callAPI :: (FromJSON v) => APIOpts -> IO (Either ErrorMessage v)
callAPI apiOpts = do
    let baseUrl = makeBaseUrl apiOpts
    let fullUrl = urlWithParams apiOpts baseUrl
    eR <- (Right <$> Nw.get fullUrl) `catchAsync` handler
    case eR of
        Left err -> return $ Left (BittrexException $ "API Error: " <>err)
        Right r -> do
            let Just (Bool success) = r ^? (Nw.responseBody . key "success")
            let Just result = r ^? Nw.responseBody . key "result"
            let Just msg = r ^? Nw.responseBody . key "message"
            pure $ if success
                then case fromJSON result of
                    Error s   -> Left (DecodeFailure (Text.pack s) result)
                    Success m -> Right m
                else case fromJSON msg of
                    Error s   -> Left (DecodeFailure (Text.pack s) msg)
                    Success m -> Left (BittrexError m)
    where
        handler :: HttpException -> IO (Either Text (Response LBS.ByteString))
        handler (HttpExceptionRequest rq (StatusCodeException  r _)) =
            return $ Left $ Text.pack $
                    show rq <>"\n"<> BSC.unpack (r ^. Nw.responseStatus . Nw.statusMessage)
        -- TODO: match all cases
        handler (HttpExceptionRequest _ _) =  return $ Left "some http exception error"
        handler (InvalidUrlException _ _) =  return $ Left "invalid url error"

