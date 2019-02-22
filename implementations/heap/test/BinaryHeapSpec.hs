module BinaryHeapSpec where

import Data.BinaryHeap
import Data.Vector (fromList)
import Test.Hspec

spec :: Spec
spec = do
  describe "insert" $ do
    it "inserts at the end of the list" $
      (insert 1 . insert 2 . insert 3 . insert 4 $ empty) `shouldBe` fromList [4, 3, 2, 1]
    it "bubbles up values when necessary after insertion" $
      (insert 4 . insert 3 . insert 2 . insert 1 $ empty) `shouldBe` fromList [4, 3, 2, 1]

  describe "extract" $ do
    it "extracts the maximum each time" $
      let (_, max) = extract . insert 5 . insert 7 . insert 10 . insert 1 . insert 3 $ empty
      in max `shouldBe` 10
    it "bubbles down values when necessary after removal" $
      let (h, _) = extract . insert 4 . insert 3 . insert 2 . insert 1 $ empty
      in h `shouldBe` fromList [3, 1, 2]
