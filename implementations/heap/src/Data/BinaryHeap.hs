module Data.BinaryHeap
    ( -- * The heap data type
      Heap
      -- * Constructors
    , empty
    , singleton
      -- * Insertion
    , insert
      -- * Extraction
    , extract
    ) where

import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V

-- | A max-heap backed by a vector
type Heap = Vector

-- | An empty heap
empty :: Heap a
empty = V.empty

-- | A heap containing one element
singleton :: a -> Heap a
singleton = V.singleton

-- | Checks if the heap is empty
null :: Heap a -> Bool
null = V.null

-- | Gets the indices of the children of a node
children :: Int -> (Int, Int)
children i = (2 * i + 1, 2 * i + 2)

-- | Gets the index of the parent of a node
parent :: Int -> Int
parent i = (i - 1) `div` 2

-- | Inserts into a heap
insert :: Ord a => a -> Heap a -> Heap a
insert x h = upheap (V.length h) (V.snoc h x)

-- | Moves an element up the heap until it is correct
upheap :: Ord a => Int -> Heap a -> Heap a
upheap i h = case h !? pi of
    Nothing -> h
    Just px -> if x > px
        then upheap pi (swap i pi h)
        else h
    where
        x = h ! i
        pi = parent i

-- | Extracts the largest element of a non-empty heap
extract :: Ord a => Heap a -> (Heap a, a)
extract h = case V.length h of
    0 -> error "empty heap"
    1 -> (empty, V.head h)
    _ -> (downHeap 0 h', V.head h)
    where
        h' = V.cons (V.last h) (V.init . V.tail $ h)

-- | Moves an element down the heap until it is correct
downHeap :: Ord a => Int -> Heap a -> Heap a
downHeap i h = if i /= i'
    then downHeap i' (swap i i' h)
    else h
    where
        i' = maximumBy (comparing (h !?)) [i, li, ri]
        (li, ri) = children i

-- | Swaps two elements
swap :: Int -> Int -> Heap a -> Heap a
swap i j h = h // [(i, h ! j), (j, h ! i)]
