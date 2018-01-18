
module TA.RSI (
        averageGain
    ,   averageLoss
    ,   firstAverage
    ,   pairDiffs
    ,   foldAverages
    ,   AverageType(..)
    ) where

import           Data.Maybe
import           Protolude


type Period = Int
data AverageType = Gain | Loss deriving (Ord, Eq, Show)

pairUp :: Num a => [a] -> Maybe [(a,a)]
pairUp []        = Just []
pairUp [x]       = Nothing
pairUp (x:x':xs) =  (:) <$> Just (x,x') <*> pairUp xs


pairDiff :: (Fractional a, Ord a)  => AverageType -> (a, a) -> a
pairDiff avType (x, y)
    | avType == Gain && diff' > 0 = diff'
    | avType == Loss && diff' < 0 = diff' * (- 1)
    | otherwise = 0
    where diff' = y - x

pairDiffs :: (Fractional a, Ord a) => AverageType -> Maybe [(a, a)] -> Maybe [a]
pairDiffs avType pairs = case pairs of
    Just xs -> Just $ fmap (pairDiff avType) xs
    Nothing -> Nothing

-- for getting first averages & losses
firstAverage :: (Fractional a, Ord a) => AverageType -> Period -> Maybe [(a, a)] -> Maybe a
firstAverage avType period pairs = div'' <$> diffsTotal
    where
        div'' = flip (/) $ fromIntegral period
         -- we only want to deal with a few pairs. 7 pairs will lead to more than 7 diffs
        diffs = pairDiffs avType $ fmap (take period) pairs
        -- ultimately we want to take from the diffs themeselves
        diffsTotal = sum . take period <$> diffs



foldAverages :: (Fractional a, Ord a) => AverageType -> Period -> a ->[(a, a)] -> a
foldAverages avType period =
    foldr(\pair acc -> let currentDiff = pairDiff avType pair
                        in (acc * 13 + currentDiff) / fromIntegral period)

aggregateAverages :: (Fractional a, Ord a) => AverageType -> Period -> [a] -> Maybe a
aggregateAverages avType period values =
    let allPairs = pairUp values
        initialAverage = firstAverage avType period allPairs
    in case initialAverage of
        Just avg -> foldAverages avType period avg <$> allPairs
        Nothing  -> Nothing

averageGain :: (Fractional a, Ord a) => Period -> [a] -> Maybe a
averageGain = aggregateAverages Gain

averageLoss :: (Fractional a, Ord a) => Period -> [a] -> Maybe a
averageLoss = aggregateAverages Loss
