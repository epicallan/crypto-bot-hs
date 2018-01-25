{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
module Utils.DB where
import           Data.Dates
import qualified Data.List          as L
import           Data.Maybe
import qualified Data.String        as S (fromString)
import qualified Data.Text          as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.MongoDB
import           Protolude
import           Types              (BasicStats (..), DipState (..))

-- | This module has db methods we use to save the script results in a db

cryptoDB :: Database
cryptoDB = "crypto"

dipCol :: Collection
dipCol = "dip"

type Coin = Text

runDb :: Action IO a -> IO a
runDb action = do
    pipe <- connect (host "localhost")
    result <- access pipe master cryptoDB action
    close pipe
    return result


-- | we insert records on a weekly basis
-- | hence we have a week field which is a date range i.e 22-01-2018 - 28-01-2018

getDate :: IO DateTime
getDate =  (toDate .toGregorian . utctDay) <$> getCurrentTime
    where
        toDate :: (Integer, Int, Int) -> DateTime
        toDate (year', month', day') = DateTime (fromIntegral year') month' day' 0 0 0


getDateRange :: IO (DateTime, DateTime) -- tuple of 2 dates
getDateRange = (\currentDate -> (currentDate, newDate currentDate)) <$> getDate
    where
        newDate :: DateTime -> DateTime
        newDate now =  addInterval now (Days 7)

newtype ValDate = ValDate DateTime deriving (Eq, Show)

instance Val ValDate where
    val (ValDate d') = String $ show d'
     -- to be fixed with parsec / just trying ot get things to
    cast' _ = Nothing

instance Val (ValDate, ValDate) where
    val (x, y) = String $ T.pack (show x ++ show y)
    --- to be fixed with proper parsec fn, i just want to test writing to DB works
    cast' _ = Nothing

addRecord :: DipState -> Action IO Value
addRecord dip = do
    range <- liftIO getDateRange
    insert dipCol (fields range)
    where
        stats' = stats dip
        toValDate (x, y) = (ValDate x, ValDate y)
        fields range' = [
                "coinName" =: coinName dip
            ,   "mean" =: mean stats'
            ,   "std" =: std stats'
            ,   "date range" =: toValDate range'
            ]

-- shouldAddRecord :: Coin -> Action IO Value
-- shouldAddRecord =
