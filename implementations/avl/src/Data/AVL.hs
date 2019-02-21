module Data.AVL
    ( -- * The tree data type
      Tree(..)
      -- * Constructors
    , empty
    , singleton
      -- * Properties
    , height
    , nullTree
      -- * Querying
    , member
    , notMember
    , count
    , minValue
    , maxValue
      -- * Insertion
    , insert
      -- * Deletion
    , delete
      -- * Mappings
    , mapTree
    , mapMonotonic
      -- * Folds
    , foldlTree
    , foldrTree
    , cataTree
      -- * List conversions
    , toAscList
    , toDescList
    , fromList
      -- * Debugging
    , showTree
    , printTree
    , valid
    ) where

import Data.AVL.Internal
