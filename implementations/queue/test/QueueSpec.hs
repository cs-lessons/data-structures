module QueueSpec where

import Data.Queue
import Test.Hspec

spec :: Spec
spec = do
  describe "fromList" $ do
    it "creates a queue from a list correctly" $
      (dequeue . fromList $ [1, 2, 3, 4]) `shouldBe` (1, fromList [2, 3, 4])

  describe "enqueue" $ do
    it "enqueues items to the back of the queue" $
      (enqueue . fromList $ [1, 2, 3, 4]) 0 `shouldBe` fromList [1, 2, 3, 4, 0]

  describe "dequeue" $ do
    it "dequeues items from the front of the queue" $
      (dequeue . fromList $ [4, 3, 2, 1]) `shouldBe` (4, fromList [3, 2, 1])
