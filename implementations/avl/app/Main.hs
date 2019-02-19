module Main where

import Data.AVL

main :: IO ()
main = do
  printTree (delete 1 . insert 4 . insert 3 . insert 2 . insert 1 $ empty)

