import           Control.Exception (evaluate)
import           Data.Maybe
import           Protolude
import           TA.RSI
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RSI" $ do
    let prices =  [2, 5, 6, 4, 2, 5, 12, 6, 4, 6, 12]
    it "should return diffs of data" $ do
      diffs (take 5 prices) `shouldBe` (Just [3, 1, -2, -2] :: Maybe [Double])
    it "should return first average" $ do
      firstAverage Gain 7 prices `shouldBe` (Just 2.0 :: Maybe Double)
    it "should return average Gain" $ do
      roundToNearest 2 <$> averageGain 7 prices `shouldBe` (Just 2.36 :: Maybe Double)
    it "should return average Loss" $ do
      roundToNearest 2 <$> averageLoss 7 prices `shouldBe` (Just 1.11 :: Maybe Double)
    it "should return rs value" $ do
        rsi 7 prices `shouldBe` (Just 68.03 :: Maybe Double)
