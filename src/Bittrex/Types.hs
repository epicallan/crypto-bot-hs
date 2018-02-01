{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------

module Bittrex.Types where

--------------------------------------------------------------------------------

import           Data.Aeson
import qualified Data.String  as S
import           Data.Text    (Text)
import qualified Data.Text    as Text
import           Data.Time
import           GHC.Generics (Generic)
import           Protolude

--------------------------------------------------------------------------------

type Params = [(Text, Text)]

--------------------------------------------------------------------------------

newtype Time
  = Time UTCTime
  deriving (Eq, Show)

instance FromJSON Time where
  parseJSON = withText "Time" $ \t -> pure (Time (parse (Text.unpack t)))
    where
      parse :: S.String -> UTCTime
      parse = parseTimeOrError True defaultTimeLocale
              $ iso8601DateFormat (Just "%H:%M:%S%Q")

data APIOpts
  = APIOpts
    {
      apiOptsQueryParams :: Params
    , apiOptsVersion     :: Text
    , apiOptsPath        :: Text
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

------------------------------------------------------------------------

data BittrexError
  = INVALID_MARKET
  | MARKET_NOT_PROVIDED
  | ADDRESS_GENERATING
  deriving (Eq, Show, Generic)

instance FromJSON BittrexError where
  parseJSON (String "INVALID_MARKET")      = pure INVALID_MARKET
  parseJSON (String "MARKET_NOT_PROVIDED") = pure MARKET_NOT_PROVIDED
  parseJSON (String "ADDRESS_GENERATING")  = pure ADDRESS_GENERATING

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
  deriving (Eq, Show)

instance FromJSON Market where
  parseJSON = withObject "Market" $ \o -> do
    marketMarketCurrency     <- o .: "MarketCurrency"
    marketBaseCurrency       <- o .: "BaseCurrency"
    marketMarketCurrencyLong <- o .: "MarketCurrencyLong"
    marketBaseCurrencyLong   <- o .: "BaseCurrencyLong"
    marketName               <- o .: "MarketName"
    marketIsActive           <- o .: "IsActive"
    marketCreated            <- o .: "Created"
    pure Market {..}

--------------------------------------------------------------------------------

data Currency
  = Currency
    { currencyName            :: Text
    , currencyNameLong        :: Text
    , currencyMinConfirmation :: Int
    , currencyIsActive        :: Bool
    , currencyCoinType        :: Text
    , currencyBaseAddress     :: Maybe Text
    }
  deriving (Eq, Show)

instance FromJSON Currency where
  parseJSON = withObject "Currency" $ \o -> do
    currencyName            <- o .:  "Currency"
    currencyNameLong        <- o .:  "CurrencyLong"
    currencyMinConfirmation <- o .:  "MinConfirmation"
    currencyIsActive        <- o .:  "IsActive"
    currencyCoinType        <- o .:  "CoinType"
    currencyBaseAddress     <- o .:? "BaseAddress"
    pure Currency {..}

--------------------------------------------------------------------------------
