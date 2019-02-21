module Data.Stack
    ( -- * The stack data type
      Stack
      -- * Constructors
    , empty
      -- * Insertion
    , push
      -- * Deletion
    , pop
      -- * Querying
    , top
      -- * Properties
    , len
      -- * Conversions
    , fromList
    , toList
    ) where

-- | A stack backed by a list
type Stack a = [a]

-- | An empty stack
empty :: Stack a
empty = []

-- | Adds an item to the top of the stack
push :: Stack a -> a -> Stack a
push = flip (:)

-- | Removes an element from the top of the stack
pop :: Stack a -> (a, Stack a)
pop [] = error "empty stack"
pop (x:xs) = (x, xs)

-- | Looks at the element at te top of the stack
top :: Stack a -> (a, Stack a)
top [] = error "empty stack"
top s@(x:xs) = (x, s)

-- | Gets the size of the stack
len :: Stack a -> Int
len = length

-- | Converts a list into a stack
fromList :: [a] -> Stack a
fromList = id

-- | Converts a stack into a list
toList :: Stack a -> [a]
toList = id
