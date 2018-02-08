module Bittrex.API where

import           Bittrex.Internal (callAPI, defOpts)
import           Bittrex.Types
import           Protolude


-- | Used to get the open and available trading markets at Bittrex along with
--   other meta data.
getMarkets :: IO (Either ErrorMessage [Market])
getMarkets = callAPI (defOpts {apiOptsPath = "getmarkets"})

-- | This function returns a summary of the last 24 hours for the given market.
getMarketSummary :: MarketName -> IO (Either ErrorMessage [MarketSummary])
getMarketSummary market =
    callAPI (
        defOpts {   apiOptsQueryParams = pure ("market", market)
                ,   apiOptsPath        = "getmarketsummary"
                }
    )

-- | This function returns historical & current coin candles by specified interval
getCandles :: TickInterval -> MarketName -> IO (Either ErrorMessage [Candle])
getCandles interval market =
    callAPI (
        defOpts {   apiOptsAPIType     =  PublicAPIV2
                ,   apiOptsVersion     = "v2.0"
                ,   apiOptsPath        = "market/GetTicks"
                ,   apiOptsQueryParams = [("marketName", market), ("tickInterval", show interval)]
            }
    )
