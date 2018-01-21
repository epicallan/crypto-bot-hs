module Utils.Stats (
    pricesAtInterval
) where

import           Data.Maybe
import           Protolude

data BasicStats = BasicStats {
        mean  :: Double
    ,   std   :: Double
    ,   slope :: Double
    ,   last  :: Double
}

-- i wonder if this could be written with foldable fns
intervalPrices :: (Num a ) => Int -> [a] -> [[a]]
intervalPrices _ [] = []
intervalPrices i p = let (x, xs) = splitAt i p
                        in x : intervalPrices i xs

pricesAtInterval :: (Num a ) => Int -> [a] -> Maybe [a]
pricesAtInterval i p =
    let priceGroups = intervalPrices i $ reverse p
    in traverse head $ reverse priceGroups

