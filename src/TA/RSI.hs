
module TA.RSI (
        firstAverage
    ,   diffs
    ,   rsi
    ,   averageGain
    ,   averageLoss
    ,   AverageType(..)
    ) where
import           Data.Maybe
import           Protolude
import           Utils.Stats (roundToNearest)


type Period = Int
data AverageType = Gain | Loss deriving (Ord, Eq, Show)
type Diffs = Maybe [Double]

diffs :: [Double] -> Diffs
diffs []        = Just []
diffs [_]       = Just []
diffs (x:x':xs) = (:) <$> Just (x' - x) <*> diffs (x' : xs)

diffsByType :: AverageType -> Diffs -> Diffs
diffsByType avType changes =
    case changes of
        Just xs -> Just $ diffByType avType  <$> xs
        Nothing -> Nothing
    where
        diffByType :: AverageType -> Double -> Double
        diffByType avType' change
            | avType' == Gain && change > 0 = change
            | avType' == Loss && change < 0 = change * (- 1)
            | otherwise = 0


-- for getting first averages & losses
firstAverage :: AverageType -> Period -> [Double] -> Maybe Double
firstAverage avType period prices = div'' <$> diffsTotal
    where
        div'' = flip (/) $ fromIntegral period
         -- we only want to deal with a few pairs. 7 pairs will lead to more than 7 diffs
        changes = diffsByType avType $ diffs prices
        -- ultimately we want to take from the diffs themeselves
        diffsTotal = sum . take period <$> changes



foldAverages :: Period -> Double -> [Double] -> Double
foldAverages period = foldr' (\change acc -> (acc * (period' - 1) + change) / period')
        where period' = fromIntegral period


aggregateAverages :: AverageType -> Period -> [Double] -> Maybe Double
aggregateAverages avType period values =
    let initialAverage = firstAverage avType period values
        allDiffs       = diffsByType avType $ diffs values
        requiredDiffs  = drop period  <$> allDiffs
    in case initialAverage of
        Just avg -> foldAverages period avg . reverse <$> requiredDiffs
        Nothing  -> Nothing

averageGain :: Period -> [Double] -> Maybe Double
averageGain = aggregateAverages Gain

averageLoss :: Period -> [Double] -> Maybe Double
averageLoss = aggregateAverages Loss

rs :: Period -> [Double] -> Maybe Double
rs period values =
    let aGain = averageGain period values
        aLoss = averageLoss period values
    in  (/) <$> aGain <*> aLoss

rsi :: Period -> [Double] -> Maybe Double
rsi period values =
    let rsValue       = rs period values
    in case rsValue of
       Just x  -> Just $ roundToNearest 2 $ 100 - (100 / (1 + x))
       Nothing -> Nothing

