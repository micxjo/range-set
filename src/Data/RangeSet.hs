{-# LANGUAGE NamedFieldPuns #-}
{-|
Module         : Data.RangeSet
Description    : A set of elments represented as a list of ranges.
Copyright      : (c) 2016 Micxjo Funkcio
License        : BSD3
Maintainer     : micxjo@fastmail.com
Stability      : experimental
-}
module Data.RangeSet
       ( -- * RangeSet type
         RangeSet
         -- * Construction
       , empty
       , singleton
       , rangeSet
       , insert
         -- * Query
       , member
       , notMember
       , size
       , isEmpty
       , isSingleton
       , ranges
         -- * Combine
       , union
       , unions
         -- * Conversion
       , toList
       , toAscList
       , toDescList
       ) where

import qualified Data.List as List

import           Data.Range (Range)
import qualified Data.Range as R
import           Data.Semigroup (Semigroup(..))

-- | A set of elements, represented as a (possibly empty) list of
-- disjoint ranges.
newtype RangeSet a = RangeSet { _ranges :: [Range a] } deriving (Eq, Show)

instance (Ord a, Enum a, Bounded a) => Semigroup (RangeSet a) where
  (<>) = union

  stimes n rs
    | n < 0 = error "stimes: RangeSet, negative multiplier"
    | n == 0 = empty
    | otherwise = rs

instance (Ord a, Enum a, Bounded a) => Monoid (RangeSet a) where
  mempty = empty
  mappend = (<>)

-- | Create an empty set.
empty :: RangeSet a
empty = RangeSet []

-- | Create a singleton set.
singleton :: Enum a => a -> RangeSet a
singleton a = RangeSet [R.singleton a]

-- | Create a set from (min a b) to (max a b), inclusive.
rangeSet :: (Ord a, Enum a) => a -> a -> RangeSet a
rangeSet a b
  | a <= b = RangeSet [R.range a b]
  | otherwise = RangeSet [R.range b a]

-- | Is the element in the set?
member :: Ord a => a -> RangeSet a -> Bool
member a RangeSet{_ranges} = any (R.member a) _ranges

-- | Is the element not in the set?
notMember :: Ord a => a -> RangeSet a -> Bool
notMember a = not . member a

-- | The sub-ranges of the set.
ranges :: RangeSet a -> [Range a]
ranges RangeSet{_ranges} = _ranges

safePred :: (Eq a, Enum a, Bounded a) => a -> a
safePred a
  | a == minBound = a
  | otherwise     = pred a

append :: (Ord a, Enum a, Bounded a) => RangeSet a -> Range a -> RangeSet a
append RangeSet{_ranges} range = RangeSet (lt ++ (mid : gt))
  where (lt, ranges') = List.partition (
          \r -> R.rangeMax r < safePred (R.rangeMin range)) _ranges
        (gt, overlap) = List.partition (
          \r -> safePred (R.rangeMin r) > R.rangeMax range) ranges'
        mid = case overlap of
          [] -> range
          _ -> foldr (<>) range overlap

-- | The union of two sets.
union :: (Ord a, Enum a, Bounded a) => RangeSet a -> RangeSet a -> RangeSet a
union rs RangeSet{_ranges} = List.foldl' append rs _ranges

-- | The union of a list of sets: (union == foldl union empty)
unions :: (Ord a, Enum a, Bounded a, Foldable t) => t (RangeSet a) -> RangeSet a
unions = List.foldl' union mempty

-- | The number of element in the set.
size :: Num a => RangeSet a -> a
size RangeSet{_ranges} = sum (map R.size _ranges)

-- | Is the set empty?
isEmpty :: RangeSet a -> Bool
isEmpty (RangeSet []) = True
isEmpty _ = False

-- | Does the set contain exactly one element?
isSingleton :: Eq a => RangeSet a -> Bool
isSingleton (RangeSet [r]) = R.isSingleton r
isSingleton _ = False

-- | Insert an elemen in the set.
insert :: (Enum a, Ord a, Bounded a) => a -> RangeSet a -> RangeSet a
insert a rs = append rs (R.singleton a)

-- | All of the elements of the set.
toList :: Enum a => RangeSet a -> [a]
toList = toAscList

-- | All of the elements of the set, in ascending order.
toAscList :: Enum a => RangeSet a -> [a]
toAscList RangeSet{_ranges} = concatMap R.toList _ranges

-- | All of the elements of the set, in descending order.
toDescList :: Enum a => RangeSet a -> [a]
toDescList RangeSet{_ranges} = concatMap R.toDescList _ranges
