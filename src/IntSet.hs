module IntSet (
    IntSet,

    empty,
    member,
    insert,
    merge,
    delete,

    fromList,
    toList
  ) where

import Data.List (foldl', nub, sort)

type IntSet = [(Int, Int)]

-- | Create an empty set
empty :: IntSet
empty = error "empty undefined"

-- | Check if an integer is in the set
member :: Int -> IntSet -> Bool
member _ _ = error "member undefined"

-- | Insert an element into the set
insert :: Int -> IntSet -> IntSet
insert _ _ = error "insert undefined"

-- | Union two sets
merge :: IntSet -> IntSet -> IntSet
merge _ _ = error "merge undefined"

-- | Delete an element from the set
delete :: Int -> IntSet -> IntSet
delete _ _ = error "delete undefined"

-- | Convert a list of @Int@s to an @IntSet@
fromList :: [Int] -> IntSet
fromList is = foldl' (flip insert) empty is

-- | Convert an @IntSet@ to a list of @Int@s
toList :: IntSet -> [Int]
toList xs = nub $ sort $ concat [[lo..hi] | (lo,hi) <- xs]
