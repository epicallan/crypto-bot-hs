module Dip (runAnalysis) where

import           Bittrex.API
import           Bittrex.Types
import           Control.Concurrent.Async
import           Data.Maybe
import qualified Data.Text                as Text
import           Protolude                hiding (onException)
import           TA.RSI                   (rsi)
import           Types
import           Utils.DB                 (addCoinDipRecord, runDb)
import           Utils.Stats              (baseStats)

-- | get actvie btc based altcoins
btcAlts :: IO (Maybe [Market])
btcAlts = getCoins
    where
        getCoins = do
            eMarkets <- getMarkets
            case eMarkets of
                Left err      -> print err >> pure Nothing
                Right markets -> pure $ Just $ requiredMarkets markets
        requiredMarkets =
            filter (\m -> marketIsActive m && marketBaseCurrency m == "BTC")

-- | get candles for a coin above a certain btc volume
btcAltCandles :: MarketName -> IO (Maybe[Candle])
btcAltCandles alt =  getBtcAltCandles
    where
        getBtcAltCandles = do
            (eSummary, eCandles) <- concurrently (getMarketSummary alt) (getCandles Day alt)
            case eSummary of
                Left err      -> print ("summary error: " ++ show err) >> pure Nothing
                Right summary' -> do
                    let summary = head summary'
                    let isAboveVolumeCap = maybe False (\x -> marketSummaryBaseVolume x > 80) summary
                    case eCandles of
                        Left err -> print ("candles error: " ++ show err) >> pure Nothing
                        Right candles ->
                            if isAboveVolumeCap then pure $ Just candles else pure Nothing


-- |  Anlyse coin and write results to DB if below a certain RSI
-- |  we want to get coins that have dipped hence we consider low RSI and
-- |  are starting to recover hence we consider positive slope

analyse :: MarketName -> IO ()
analyse alt = do
    putStrLn $ "alt: " <> alt
    candles <- btcAltCandles alt
    let closePrices = maybe [] (fmap close) candles :: [Double]
    let rsi' = fromMaybe 0 $ rsi 14 closePrices :: Double
    let stats' = baseStats closePrices :: Maybe BasicStats
    case stats' of
        Nothing -> print $ "No results for: " <> Text.unpack alt >> pure ()
        Just stats'' -> do
            print $ "ris: " ++ show rsi'
            print $ "stats: " ++ show stats'
            let dipState = DipState alt stats'' rsi'
            let change'  = change stats''
            let slope'   = slope stats''
            let hasDipped = rsi' < 45 && rsi' > 0 && change' > 40 && slope' > 0
            when hasDipped (void $ runDb (addCoinDipRecord dipState))


runAnalysis :: IO ()
runAnalysis = do
    alts <- btcAlts
    case alts of
        Nothing    -> pure ()
        Just coins -> mapM_ (analyse . marketName) coins
