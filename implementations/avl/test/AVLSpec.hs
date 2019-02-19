module AVLSpec where

import Data.AVL
import Test.Hspec

spec :: Spec
spec = do
  describe "insert" $ do
    it "performs RR rotations correctly on insertion" $
      height (insert 1 . insert 2 . insert 3 $ empty) `shouldBe` 1
    it "performs RL rotations correctly on insertion" $
      height (insert 1 . insert 3 . insert 2 $ empty) `shouldBe` 1
    it "performs LR rotations correctly on insertion" $
      height (insert 3 . insert 1 . insert 2 $ empty) `shouldBe` 1
    it "performs LL rotations correctly on insertion" $
      height (insert 3 . insert 2 . insert 1 $ empty) `shouldBe` 1

  describe "delete" $ do
    it "performs RR rotations when necessary after deletion" $
      height (delete 1 . insert 4 . insert 3 . insert 1 . insert 2 $ empty) `shouldBe` 1
    it "performs RL rotations when necessary after deletion" $
      height (delete 1 . insert 3 . insert 4 . insert 1 . insert 2 $ empty) `shouldBe` 1
    it "performs LR rotations when necessary after deletion" $
      height (delete 4 . insert 2 . insert 4 . insert 1 . insert 3 $ empty) `shouldBe` 1
    it "performs LL rotations when necessary after deletion" $
      height (delete 4 . insert 1 . insert 4 . insert 2 . insert 3 $ empty) `shouldBe` 1
    it "rotates as many times as necessary for balance after deletion" $
      height (delete 'l' (foldr insert empty "aigbcklfdjhe")) `shouldBe` 3
