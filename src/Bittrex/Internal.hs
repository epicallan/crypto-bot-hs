{-# LANGUAGE RecordWildCards #-}

module Bittrex.Internal (
        callAPI
    ,   defOpts) where

import           Bittrex.Types
import           Data.Aeson
import           Data.List
import           Data.String      (String)
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Lens.Micro
import           Lens.Micro.Aeson
import           Network.Wreq
import           Protolude        hiding (get)

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
    r <- get fullUrl
    let Just (Bool success) = r ^? (responseBody . key "success")
    let Just result = r ^? responseBody . key "result"
    let Just msg = r ^? responseBody . key "message"
    pure $ if success
        then case fromJSON result of
            Error s   -> Left (DecodeFailure (Text.pack s) result)
            Success m -> Right m
        else case fromJSON msg of
            Error s   -> Left (DecodeFailure (Text.pack s) msg)
            Success m -> Left (BittrexError m)

