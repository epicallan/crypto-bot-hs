
module TA.RSI (
        firstAverage
    ,   diffs
    ,   rsi
    ,   roundToNearest
    ,   averageGain
    ,   averageLoss
    ,   AverageType(..)
    ) where
import           Data.Maybe
import           Protolude


type Period = Int
data AverageType = Gain | Loss deriving (Ord, Eq, Show)
type Diffs a = Maybe [a]

diffs :: Num a => [a] -> Diffs a
diffs []        = Just []
diffs [_]       = Just []
diffs (x:x':xs) = (:) <$> Just (x' - x) <*> diffs (x' : xs)

diffsByType :: (RealFrac a, Ord a) => AverageType -> Diffs a -> Diffs a
diffsByType avType changes =
    case changes of
        Just xs -> Just $ diffByType avType  <$> xs
        Nothing -> Nothing
    where
        diffByType :: (RealFrac a, Ord a) => AverageType -> a -> a
        diffByType avType' change
            | avType' == Gain && change > 0 = change
            | avType' == Loss && change < 0 = change * (- 1)
            | otherwise = 0


-- for getting first averages & losses
firstAverage ::(RealFrac a) => AverageType -> Period -> [a] -> Maybe a
firstAverage avType period prices = div'' <$> diffsTotal
    where
        div'' = flip (/) $ fromIntegral period
         -- we only want to deal with a few pairs. 7 pairs will lead to more than 7 diffs
        changes = diffsByType avType $ diffs prices
        -- ultimately we want to take from the diffs themeselves
        diffsTotal = sum . take period <$> changes



foldAverages :: (RealFrac a, Ord a) => Period -> a -> [a] -> a
foldAverages period = foldr' (\change acc -> (acc * (period' - 1) + change) / period')
        where period' = fromIntegral period


aggregateAverages :: (RealFrac a, Ord a) => AverageType -> Period -> [a] -> Maybe a
aggregateAverages avType period values =
    let initialAverage = firstAverage avType period values
        allDiffs       = diffs values
        allGains       = diffsByType avType allDiffs
        priceDiffs     = drop period  <$> allGains
    in case initialAverage of
        Just avg -> foldAverages period avg . reverse <$> priceDiffs
        Nothing  -> Nothing

averageGain :: (RealFrac a, Ord a) => Period -> [a] -> Maybe a
averageGain = aggregateAverages Gain

averageLoss :: (RealFrac a, Ord a) => Period -> [a] -> Maybe a
averageLoss = aggregateAverages Loss

rs :: (RealFrac a, Ord a) => Period -> [a] -> Maybe a
rs period values =
    let aGain = averageGain period values
        aLoss = averageLoss period values
    in  (/) <$> aGain <*> aLoss

roundToNearest :: (RealFrac a) => Int -> a -> a
roundToNearest n x = fromIntegral (round (x * 10^n)) / 100

rsi :: (RealFrac a, Ord a) => Period -> [a] -> Maybe a
rsi period values =
    let rsValue       = rs period values
    in case rsValue of
       Just x  -> Just $ roundToNearest 2 $ 100 - (100 / (1 + x))
       Nothing -> Nothing

