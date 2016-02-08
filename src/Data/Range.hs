{-|
Module         : Data.Range
Description    : An inclusive, non-empty range.
Copyright      : (c) 2016 Micxjo Funkcio
License        : BSD3
Maintainer     : micxjo@fastmail.com
Stability      : experimental
-}
module Data.Range
       ( -- * Range type
         Range
         -- * Construction
       , singleton
       , range
         -- * Query
       , member
       , notMember
       , rangeMin
       , rangeMax
       , isSingleton
       , size
         -- * Combine
       , intersection
         -- * Conversion
       , toList
       , toAscList
       , toDescList
       ) where

import Data.Semigroup (Semigroup(..))

-- | An inclusive, non-empty range.
data Range a = Range
               { _start :: !a
               , _end :: !a
               } deriving (Eq, Show)

-- | Create a singleton range.
singleton :: a -> Range a
singleton a = Range a a

-- | Create a range from (min a b) to (max a b), inclusive.
range :: Ord a => a -> a -> Range a
range start end
  | start <= end = Range start end
  | otherwise = Range end start

instance Ord a => Semigroup (Range a) where
  (<>) (Range start1 end1) (Range start2 end2) =
    Range (min start1 start2) (max end1 end2)

  stimes n r
    | n <= 0 = error "stimes: positive multiplier expected"
    | otherwise = r

-- | The intersection of two ranges. If the ranges are disjoint, Nothing.
intersection :: Ord a => Range a -> Range a -> Maybe (Range a)
intersection (Range start1 end1) (Range start2 end2) =
  if start > end
     then Nothing
     else Just (Range start end)
  where start = max start1 start2
        end = min end1 end2

overlap :: Ord a => Range a -> Range a -> Bool
overlap (Range start1 end1) (Range start2 end2) =
  (start1 >= start2 && start1 <= end2) ||
  (start2 >= start1 && start2 <= end1)

-- | The minimum element of the range.
rangeMin :: Range a -> a
rangeMin (Range a _) = a

-- | The maximum element of the range.
rangeMax :: Range a -> a
rangeMax (Range _ a) = a

-- | The number of elements in the range.
size :: Num a => Range a -> a
size (Range start end) = end - start + 1

-- | Does the range contain only  a single element?
isSingleton :: Eq a => Range a -> Bool
isSingleton (Range a b) = a == b

-- | All of the elements of the range.
toList :: Enum a => Range a -> [a]
toList (Range start end) = [start..end]

-- | All of the elemens of the range, in ascending order.
toAscList :: Enum a => Range a -> [a]
toAscList = toList

-- | All of the elements of the range, in descending order.
toDescList :: Enum a => Range a -> [a]
toDescList = reverse . toList

-- | Is the element in the range?
member :: Ord a => a -> Range a -> Bool
member a (Range start end) = a >= start && a <= end

-- | Is the element not in the range?
notMember :: Ord a => a -> Range a -> Bool
notMember a = not . member a
