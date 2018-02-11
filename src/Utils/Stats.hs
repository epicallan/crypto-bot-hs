module Utils.Stats (
        pricesAtInterval
    ,   baseStats
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

-- FIXME: correct should return a maybe
mean' :: [Double] -> Double
mean' x = sum x / fromIntegral (length x)

-- FIXME: correct should return a maybe
std' :: [Double] -> Double
std' x =
    let μ       = mean' x
        length' = fromIntegral (length x)
        var     = foldr(\c acc -> (μ - c)**2 + acc) 0 x
    in sqrt (var / length')


-- FIXME: correct should return a maybe
slope' :: [Double] -> Double
slope' y =
    let length' = fromIntegral $ L.length y :: Double
        xs      = U.fromList [0..length']
        ys      = U.fromList y
        (_, beta) = linearRegression xs ys
    in beta

roundToNearest :: Int -> Double -> Double
roundToNearest n x = numerator' / 10^n
    where
        numerator' :: Double
        numerator' = fromInteger (round (x * 10^n))


baseStats :: [Double] -> Maybe BasicStats
baseStats [] = Nothing
baseStats x  =
    let last'   = L.last x
        roundTo' = roundToNearest 2
    in Just $ BasicStats (roundTo' $ mean' x)  (roundTo' $ std' x) (roundTo' $ slope' x) last'
