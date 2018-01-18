import           Control.Exception (evaluate)
import           Data.Maybe
import           Protolude
import           TA.RSI
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let pricePairs =  [(2, 5), (2, 3), (2, 6), (4, 6), (5, 7), (4, 3), (2, 3), (4, 6)]
  describe "Sum of price pairs" $ do
    it "should return diffs of pairs" $ do
      let xs = Just $ take 3 pricePairs
      pairDiffs Gain xs `shouldBe` (Just [3, 1, 4] :: Maybe [Double])
    -- it "should return first average" $ do
    --   firstAverage Gain 3 (Just pricePairs) `shouldBe` (Just 2 :: Maybe Double)
