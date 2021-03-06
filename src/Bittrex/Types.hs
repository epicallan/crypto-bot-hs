
module Bittrex.Types where

--------------------------------------------------------------------------------

import           Data.Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.String      (String)
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Time
import           GHC.Generics     (Generic)
import           GHC.Show 
import           Protolude hiding  (show)

--------------------------------------------------------------------------------

type MarketName = Text

type Params = [(Text, Text)]

--------------------------------------------------------------------------------

newtype Time
  = Time UTCTime
  deriving (Eq, Show)

instance FromJSON Time where
  parseJSON = withText "Time" $ \t -> pure (Time (parse (Text.unpack t)))
    where
      parse :: String -> UTCTime
      parse = parseTimeOrError True defaultTimeLocale
              $ iso8601DateFormat (Just "%H:%M:%S%Q")

data TickInterval
  = OneMin
  | FiveMin
  | ThirtyMin
  | Hour
  | Day
  deriving (Eq)

instance Show TickInterval where
  show OneMin    = "oneMin"
  show FiveMin   = "fiveMin"
  show ThirtyMin = "thirtyMin"
  show Hour      = "hour"
  show Day       = "day"

data APIType
  = PublicAPI
  | PublicAPIV2
  | AccountAPI
  | MarketAPI
  deriving (Eq)

instance Show APIType where
  show AccountAPI  = "account"
  show PublicAPI   = "public"
  show PublicAPIV2 = "pub"
  show MarketAPI   = "market"

data APIOpts
  = APIOpts
    { apiOptsAPIType     :: APIType
    , apiOptsQueryParams :: Params
    , apiOptsVersion     :: Text
    , apiOptsPath        :: Text
    }
  deriving (Eq, Show)

data ErrorMessage
  = BittrexError BittrexError
  | DecodeFailure Text Aeson.Value
  | BittrexException Text
  deriving (Eq, Show, Generic)


data BittrexError
  = INVALID_MARKET
  | MARKET_NOT_PROVIDED
  | ADDRESS_GENERATING
  | SOME_ERROR Text
  deriving (Eq, Show, Generic)

getSomeError :: Show a => a -> BittrexError
getSomeError = SOME_ERROR . Text.pack . show

instance FromJSON BittrexError where
  parseJSON (String "INVALID_MARKET")      = pure INVALID_MARKET
  parseJSON (String "MARKET_NOT_PROVIDED") = pure MARKET_NOT_PROVIDED
  parseJSON (String "ADDRESS_GENERATING")  = pure ADDRESS_GENERATING
  parseJSON (String p)                     = pure (SOME_ERROR p)
  parseJSON _                              = pure (SOME_ERROR "some strange bittrex error")


--------------------------------------------------------------------------------

data Ticker
  = Ticker
    { tickerBid  :: Double
    , tickerAsk  :: Double
    , tickerLast :: Double
    }
  deriving (Show, Generic)

instance FromJSON Ticker where
  parseJSON = withObject "Ticker" $ \o -> do
    tickerBid  <- o .: "Bid"
    tickerAsk  <- o .: "Ask"
    tickerLast <- o .: "Last"
    pure Ticker {..}

data Market
  = Market
    { marketMarketCurrency     :: Text
    , marketBaseCurrency       :: Text
    , marketMarketCurrencyLong :: Text
    , marketBaseCurrencyLong   :: Text
    , marketName               :: Text
    , marketIsActive           :: Bool
    , marketCreated            :: Time
    }
  deriving (Eq, Show, Generic)

addMarketPrefix :: String -> String 
addMarketPrefix label =  "market" ++ label

instance FromJSON Market where
  parseJSON = genericParseJSON  $ defaultOptions { fieldLabelModifier = addMarketPrefix }

--------------------------------------------------------------------------------

---------------------------------------------------------------------
data MarketSummary
  = MarketSummary
    { marketSummaryMarketName        :: MarketName
    , marketSummaryHigh              :: Double
    , marketSummaryLow               :: Double
    , marketSummaryVolume            :: Double
    , marketSummaryLast              :: Double
    , marketSummaryBaseVolume        :: Double
    , marketSummaryBid               :: Double
    , marketSummaryAsk               :: Double
    , marketSummaryOpenBuyOrders     :: Int
    , marketSummaryOpenSellOrders    :: Int
    , marketSummaryCreated           :: Time
    , marketSummaryDisplayMarketName :: Maybe Text
    }
  deriving (Eq, Show, Generic)


addSummaryPrefix :: String -> String 
addSummaryPrefix label =  "marketSummary" ++ label

instance FromJSON MarketSummary where
  parseJSON = genericParseJSON  $ defaultOptions { fieldLabelModifier = addSummaryPrefix }

data Candle
    = Candle
      { open   :: Double
      , high   :: Double
      , low    :: Double
      , close  :: Double
      , time   :: Time
      , volume :: Double
      }
    deriving (Eq, Show, Generic)

instance FromJSON Candle where
  parseJSON = withObject "Candle" $ \o -> do
    open    <- o  .: "O"
    high    <- o  .: "H"
    low     <- o  .: "L"
    close   <- o  .: "C"
    time    <- o  .: "T"
    volume  <- o  .: "V"
    pure Candle {..}
