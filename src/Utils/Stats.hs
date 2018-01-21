module Utils.Stats (
        pricesAtInterval
    ,   baseStats
    ,   BasicStats(..)
) where

import qualified Data.List                   as L
import           Data.Maybe
import qualified Data.Vector.Unboxed         as U
import           Protolude
import           Statistics.LinearRegression

data BasicStats = BasicStats {
        mean    :: Double
    ,   std     :: Double
    ,   slope   :: Double
    ,   current :: Double
} deriving (Show)

-- i wonder if this could be written with foldable fns
intervalPrices :: Num a  => Int -> [a] -> [[a]]
intervalPrices _ [] = []
intervalPrices i p = let (x, xs) = splitAt i p
                        in x : intervalPrices i xs

pricesAtInterval :: (Num a ) => Int -> [a] -> Maybe [a]
pricesAtInterval i p =
    let priceGroups = intervalPrices i $ reverse p
    in traverse head $ reverse priceGroups


mean' :: [Double] -> Double
mean' x = sum x / fromIntegral (length x)

std' :: [Double] -> Double
std' x =
    let μ       = mean' x -- 4.25
        length' = fromIntegral (length x) -- 4
        var     = foldr(\c acc -> (μ - c)**2 + acc) 0 x -- 0.25 - 1.75 - 0.75 2.25
    in sqrt (var / length')



slope' :: [Double] -> Double
slope' y =
    let length' = fromIntegral $ L.length y :: Double
        xs      = U.fromList [0..length']
        ys      = U.fromList y
        (_, beta) = linearRegression xs ys
    in beta

baseStats :: [Double] -> Maybe BasicStats
baseStats [] = Nothing
baseStats x  =
    let last' = L.last x
    in Just $ BasicStats (mean' x)  (std' x) (slope' x) last'