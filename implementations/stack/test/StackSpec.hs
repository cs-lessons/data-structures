module StackSpec where

import Data.Stack
import Test.Hspec

spec :: Spec
spec = do
  describe "fromList" $ do
    it "creates a stack from a list correctly" $
      (pop . fromList $ [1, 2, 3, 4]) `shouldBe` (1, fromList [2, 3, 4])

  describe "push" $ do
    it "pushes items to the top of the stack" $
      (push . fromList $ [1, 2, 3, 4]) 0 `shouldBe` fromList [0, 1, 2, 3, 4]

  describe "pop" $ do
    it "pops items from the top of the stack" $
      (pop . fromList $ [4, 3, 2, 1]) `shouldBe` (4, fromList [3, 2, 1])
