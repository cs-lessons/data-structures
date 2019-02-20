module Data.Queue
    ( -- * The queue data type
      Queue
      -- * Constructors
    , empty
      -- * Insertion
    , enqueue
      -- * Deletion
    , dequeue
      -- * Querying
    , front
    , contains
      -- * Properties
    , len
      -- * Conversions
    , fromList
    , toList
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | A queue backed by a vector
type Queue = Vector

-- | An empty queue
empty :: Queue a
empty = V.empty

-- | Adds an item to the back of the queue
enqueue :: Queue a -> a -> Queue a
enqueue = V.snoc

-- | Removes the element at the front of the queue
dequeue :: Queue a -> (a, Queue a)
dequeue q = case V.length q of
    0 -> error "empty queue"
    _ -> (V.head q, V.tail q)

-- | Looks at the element at the front of the queue
front :: Queue a -> (a, Queue a)
front q = case V.length q of
    0 -> error "empty queue"
    _ -> (V.head q, q)

-- | Checks whether the queue has a specific element
contains :: Eq a => a -> Queue a -> Bool
contains = V.elem

-- | Gets the length of the queue
len :: Queue a -> Int
len = V.length

-- | Converts a list into a queue
fromList :: [a] -> Queue a
fromList = V.fromList

-- | Converts a queue into a list
toList :: Queue a -> [a]
toList = V.toList
