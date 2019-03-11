module IntSetPropertySpec (
    spec
  ) where

import Data.List
import Test.Hspec
import Test.QuickCheck

import IntSet (IntSet)
import qualified IntSet as IS

newtype IS = IntSet IntSet
  deriving (Eq, Show)

instance Arbitrary IS where
  arbitrary = do xs <- sized (\s -> arbIntSet s 0)
                 return (IntSet xs)
    where
      arbIntSet :: Int -- ^ Size
                -> Int -- ^ Lower bound
                -> Gen IntSet
      arbIntSet 0    _  = return []
      arbIntSet size lb = frequency [(1,    return []),
                                     (size, arbIntSetRec)]
        where
          arbIntSetRec :: Gen IntSet
          arbIntSetRec = do
              NonNegative lo    <- fmap (`mod` 100) <$> arbitrary
              NonNegative delta <- fmap (`mod` 100) <$> arbitrary
              xs <- arbIntSet (size-1) (lo+delta+lb+2)
              return ((lo+lb,lo+delta+lb):xs)

  shrink (IntSet xs) = map IntSet $ removeOne xs ++ shrinkOne xs
    where
      removeOne :: [a] -> [[a]]
      removeOne []     = []
      removeOne [x]    = [[]]
      removeOne (x:xs) = xs : map (x:) (removeOne xs)

      shrinkOne :: [(Int,Int)] -> [[(Int,Int)]]
      shrinkOne []                       = []
      shrinkOne ((lo,hi):xs) | hi == lo  = shrinkOne xs
                             | otherwise = ((lo+1,hi):xs) :
                                           ((lo,hi-1):xs) :
                                           map ((lo,hi):) (shrinkOne xs)

spec :: Spec
spec = do
  describe "IntSet Property Tests" $ do
    it "valid: Arbitrary IntSet is valid" $
      property prop_valid
    it "empty: model of empty set is empty" $
      property prop_empty
    it "member: x is a member of set iff it is a member of the model" $
      property prop_member
    it "merge: merge of two sets equal to merge of model" $
      property prop_merge
    it "insert: inserting into set equivalent to inserting into model" $
      property prop_insert
    it "delete: deleting from set equivalent to deleting from model" $
      property prop_delete

-- | Model an IntSet by a list of Ints in the set
model :: IntSet -> [Int]
model xs = normalize $ concat [[lo..hi] | (lo,hi) <- xs]

-- | Normalize a list of integers so that it is sorted without duplicates
normalize :: [Int] -> [Int]
normalize = nub . sort

-- | The IntSet invariant from the problem description
invariant :: IntSet -> Bool
invariant xs =
    all (\(lo,hi) -> lo <= hi) xs &&
    all (\((_,hi), (lo,_)) -> lo > hi + 1) (xs `zip` drop 1 xs)

prop_valid :: IS -> Bool
prop_valid (IntSet xs) = invariant xs

prop_empty :: Bool
prop_empty = error "prop_empty: undefined"

prop_member :: Int -> IS -> Bool
prop_member = error "prop_member: undefined"

prop_merge :: IS -> IS -> Bool
prop_merge = error "prop_merge: undefined"

prop_insert :: Int -> IS -> Bool
prop_insert = error "prop_insert: undefined"

prop_delete :: Int -> IS -> Bool
prop_delete = error "prop_delete: undefined"
