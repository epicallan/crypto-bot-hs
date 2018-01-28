
module Types (
        DipState(..)
    ,   BasicStats(..)
    ) where

import           Protolude

data BasicStats = BasicStats {
        mean    :: Double
    ,   std     :: Double
    ,   slope   :: Double
    ,   current :: Double
} deriving (Show)

data DipState = DipState {
        coinName :: Text
    ,   stats    :: BasicStats
} deriving(Show)


