module Dip where

import           Bittrex.API
import           Bittrex.Types
import           Control.Exception.Safe
import           Data.Maybe
import qualified Data.Text              as Text
import           Protolude              hiding (onException)
import           System.Exit
import           TA.RSI

-- | get coins that have dipped & are possibly in consolidation
btcAlts :: IO (Maybe [Market])
btcAlts = getCoins `onException` exitWith (ExitFailure 500)
    where
        getCoins = do
            emarkets <- getMarkets
            case emarkets of
                Left err      -> print err >> pure Nothing
                Right markets -> pure $ Just $ requiredMarkets markets
        requiredMarkets =
            filter (\m -> marketIsActive m && marketBaseCurrency m == "BTC")
--- get btc / atl coins that are active and above a certain btc value

-- btcAlts :: IO (Maybe [Market])
-- btcAlts = do
--     eMarkets <- getMarkets
--     case eMarkets of
--         Left error -> do put

-- get coin summary & coin candles

--
