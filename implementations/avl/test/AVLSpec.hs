module AVLSpec where

import Data.AVLimpl
import Test.Hspec

spec :: Spec
spec = do
  describe "insert" $ do
    it "performs RR rotations correctly on insertion" $
      (insert 3 . insert 2 . insert 1 $ empty) `shouldBe` (Node 1 2 (Node 0 1 Empty Empty) (Node 0 3 Empty Empty))
    it "performs RL rotations correctly on insertion" $
      (insert 2 . insert 3 . insert 1 $ empty) `shouldBe` (Node 1 2 (Node 0 1 Empty Empty) (Node 0 3 Empty Empty))
    it "performs LR rotations correctly on insertion" $
      (insert 2 . insert 1 . insert 3 $ empty) `shouldBe` (Node 1 2 (Node 0 1 Empty Empty) (Node 0 3 Empty Empty))
    it "performs LL rotations correctly on insertion" $
      (insert 1 . insert 2 . insert 3 $ empty) `shouldBe` (Node 1 2 (Node 0 1 Empty Empty) (Node 0 3 Empty Empty))

  describe "delete" $ do
    it "performs RR rotations when necessary after deletion" $
      (delete 1 . insert 4 . insert 3 . insert 1 . insert 2 $ empty) `shouldBe` (Node 1 3 (Node 0 2 Empty Empty) (Node 0 4 Empty Empty))
    it "performs RL rotations when necessary after deletion" $
      (delete 1 . insert 3 . insert 4 . insert 1 . insert 2 $ empty) `shouldBe` (Node 1 3 (Node 0 2 Empty Empty) (Node 0 4 Empty Empty))
    it "performs LR rotations when necessary after deletion" $
      (delete 4 . insert 2 . insert 4 . insert 1 . insert 3 $ empty) `shouldBe` (Node 1 2 (Node 0 1 Empty Empty) (Node 0 3 Empty Empty))
    it "performs LL rotations when necessary after deletion" $
      (delete 4 . insert 1 . insert 4 . insert 2 . insert 3 $ empty) `shouldBe` (Node 1 2 (Node 0 1 Empty Empty) (Node 0 3 Empty Empty))
    it "rotates as many times as necessary for balance after deletion" $
      (delete 'l' (foldr insert empty "aigbcklfdjhe")) `shouldBe` (Node 3 'e' (Node 2 'c' (Node 1 'b' (Node 0 'a' Empty Empty) Empty) (Node 0 'd' Empty Empty)) (Node 2 'h' (Node 1 'f' Empty (Node 0 'g' empty empty)) (Node 1 'j' (Node 0 'i' Empty Empty) (Node 0 'k' Empty Empty))))
