module Utils.Stats (
        pricesAtInterval
    ,   baseStats
    ,   pricePercChange
    ,   slope'
    ,   std'
    ,   mean
    ,   roundToNearest
) where

import qualified Data.List                   as L
import           Data.Maybe
import qualified Data.Vector.Unboxed         as U
import           Protolude
import           Statistics.LinearRegression
import           Types                       (BasicStats (..))

-- i wonder if this could be written with foldable fns
intervalPrices :: Num a  => Int -> [a] -> [[a]]
intervalPrices _ [] = []
intervalPrices i p = let (x, xs) = splitAt i p
                        in x : intervalPrices i xs

pricesAtInterval :: (Num a ) => Int -> [a] -> Maybe [a]
pricesAtInterval i p =
    let priceGroups = intervalPrices i $ reverse p
    in traverse head $ reverse priceGroups

mean :: [Double] -> Maybe Double
mean [] = Nothing
mean x  = Just $  sum x / fromIntegral (length x)

std' :: [Double] -> Maybe Double
std' [] = Nothing
std' x  =
    let maybeμ     = mean x
        length'    = fromIntegral (length x)
        maybeVar   = case maybeμ of
                        Nothing -> Nothing
                        Just μ  -> Just $ foldr(\c acc -> (μ - c)**2 + acc) 0 x
    in case maybeVar of
        Nothing  -> Nothing
        Just var -> Just $ sqrt (var / length')

slope' :: [Double] -> Maybe Double
slope' [] = Nothing
slope' y  =
    let length' = fromIntegral $ L.length y :: Double
        xs      = U.fromList [0..length']
        ys      = U.fromList y
        (_, beta) = linearRegression xs ys
    in Just beta

roundToNearest :: Int -> Double -> Double
roundToNearest n x = fromInteger (round (x * 10^n)) / 10^n

pricePercChange :: [Double] -> Maybe Double
pricePercChange [] = Nothing
pricePercChange x  =
    let maxPrice = maximum x -- maximum price for say past month
        currentAvgPrice' = mean $ reverse $ take 3 (reverse x)-- avg price for past 3 days
    in case currentAvgPrice' of
        Nothing              -> Nothing
        Just currentAvgPrice -> Just $ ((maxPrice - currentAvgPrice) / maxPrice) * 100

baseStats :: [Double] -> Maybe BasicStats
baseStats [] = Nothing
baseStats x  =
    let x'            = reverse x
        reverseTake y = reverse . take y -- i dont want to use drop
        direction     = roundToNearest 2 $ (* 1e5) $ fromMaybe 0 $ slope' (reverseTake 5 x')
        std''         = roundToNearest 2 $ (* 1e5) $ fromMaybe 0 $ std' (reverseTake 7 x')
        change'       = roundToNearest 2 $ fromMaybe 0  $ pricePercChange $ reverseTake 24 x'
    in Just $ BasicStats direction change' std''
