
module TA.RSI (pairUp) where

import           Data.Maybe
import           Protolude


pairUp :: Fractional a => [a] -> Maybe [(a,a)]
pairUp []        = Just []
pairUp [x]       = Nothing
pairUp (x:x':xs) =  (:) <$> Just (x,x') <*> pairUp xs


sumPairs :: (Fractional a, Ord a) => Maybe [(a, a)] -> Maybe a
sumPairs pairs = case pairs of
    Just xs -> Just $ sumOfPairs xs
    Nothing -> Nothing
    where
        sumOfPairs = foldr (\tupel acc -> let (x, y) = tupel
                                              gain = y - x
                                          in if gain > 0
                                                then gain + acc
                                                else 0
                                    ) 0


averageGain :: (Fractional a, Ord a) => Int -> [a] -> Maybe a
averageGain period values = div'' <$> totalGains
    where
        totalGains =  sumPairs $ pairUp $ take period values
        div'' = flip (/) $ fromIntegral period
