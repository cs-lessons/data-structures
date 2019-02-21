module BSTSpec where

import Data.BST.Internal
import Test.Hspec

spec :: Spec
spec = do
  describe "insert" $ do
    it "inserts into an empty tree correctly" $
      insert 1 empty `shouldBe` Node 0 1 Empty Empty
    it "inserts into a populated tree correctly" $
      (insert 4 . insert 1 . insert 3 . insert 2 $ empty) `shouldBe` (Node 2 2 (Node 0 1 Empty Empty) (Node 1 3 Empty (Node 0 4 Empty Empty)))

  describe "delete" $ do
    it "handles the leaf case correctly" $
      (delete 1 . insert 4 . insert 3 . insert 1 . insert 2 $ empty) `shouldBe` (Node 2 2 Empty (Node 1 3 Empty (Node 0 4 Empty Empty)))
    it "handles the left child only case correctly" $
      (delete 1 . fromList $ [0, 3, 4, 1, 2]) `shouldBe` (Node 2 2 (Node 0 0 Empty Empty) (Node 1 4 (Node 0 3 Empty Empty) Empty))
    it "handles the right child only case correctly" $
      (delete 3 . insert 4 . insert 3 . insert 1 . insert 2 $ empty) `shouldBe` (Node 1 2 (Node 0 1 Empty Empty) (Node 0 4 Empty Empty))
    it "handles the case with both children using the successor replacement strategy" $
      (delete 8 . fromList $ [9, 10, 6, 7, 8, 5]) `shouldBe` (Node 3 5 Empty (Node 2 9 (Node 1 7 (Node 0 6 Empty Empty) Empty) (Node 0 10 Empty Empty)))
