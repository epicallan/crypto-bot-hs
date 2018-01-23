{-# LANGUAGE ExtendedDefaultRules #-}
module Utils.DB where
import           Data.Dates
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.MongoDB
import           Protolude

-- | This module has db methods we use to save the script results in a db

cryptoDB :: Database
cryptoDB = "crypto"

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
        newDate current =  addInterval current (Days 7)
