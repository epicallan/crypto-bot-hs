
module Types (
        DipState(..)
    ,   BasicStats(..)
    ) where

import           Protolude

data BasicStats = BasicStats {
        slope  :: Double -- | direction in price for past 3 days in price
    ,   change :: Double -- | percentage change in price over 4 weeks
    ,   std    :: Double -- | std for 2 weeks prices
} deriving (Show)

data DipState = DipState {
        coinName :: Text
    ,   stats    :: BasicStats
    ,   rsiValue :: Double
} deriving(Show)
