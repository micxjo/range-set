import           Data.Semigroup ((<>))
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import           Data.Range (Range)
import qualified Data.Range as Range
import           Data.RangeSet

instance (QC.Arbitrary a, Ord a, Enum a) => QC.Arbitrary (Range a) where
  arbitrary = Range.range <$> QC.arbitrary <*> QC.arbitrary

instance (QC.Arbitrary a, Ord a, Bounded a, Enum a)
         => QC.Arbitrary (RangeSet a) where
  arbitrary = unions . map (uncurry rangeSet) <$> QC.arbitrary

rangeSemigroup :: TestTree
rangeSemigroup = testGroup "Range semigroup laws"
  [ QC.testProperty "a <> (b <> c) == (a <> b) <> c" $
    \a b c -> (a :: Range Int) <> (b <> c) == (a <> b) <> c
  ]

rangeSetMonoid :: TestTree
rangeSetMonoid = testGroup "RangeSet monoid laws"
  [ QC.testProperty "mappend mempty a = a" $
    \a -> mappend mempty (a :: RangeSet Int) == a

  , QC.testProperty "mappend a mempty = a" $
    \a -> mappend (a :: RangeSet Int) mempty == a

  , QC.testProperty "mappend a (mappend b c) == mappend (mappend a b) c" $
    \a b c -> (mappend (a :: RangeSet Int) (mappend b c)
               == mappend (mappend a b) c)
  ]

properties :: TestTree
properties = testGroup "properties"
  [ rangeSemigroup

  , rangeSetMonoid

  , QC.testProperty "member _ mempty = False" $
    \a -> not (member (a :: Int) mempty)

  , QC.testProperty "member _ (rangeSet minBound maxBound) = True" $
    \a -> member a (rangeSet (minBound :: Int) (maxBound :: Int))

  , QC.testProperty "member a (singleton a) = True" $
    \a -> member (a :: Int) (singleton a)

  , QC.testProperty "union a a = a" $
    \rs -> union (rs :: RangeSet Int) rs == rs

  , QC.testProperty "ascending property" $
    \rs -> let rs' = ranges (rs :: RangeSet Int)
               pairs = zip rs' (tail rs')
           in all (\(r1, r2) ->
                    Range.rangeMin r2 > succ (Range.rangeMax r1)) pairs

  , QC.testProperty "Range.toList" $
    \a b -> Range.toList (Range.range (a :: Int) b) == [(min a b)..(max a b)]

  , QC.testProperty "toList" $
    \rs -> let rs' = ranges (rs :: RangeSet Int)
           in toAscList rs == concatMap Range.toAscList rs'
  ]

-- Tests adapted from Rust's regex_syntax::CharClass tests.
charSetTests :: TestTree
charSetTests = testGroup "Character set tests"
  [ testCase "union, ordrered" $ do
      let rs = union (rangeSet 'a' 'c') (rangeSet 'x' 'z')
      ranges rs @?= [Range.range 'a' 'c', Range.range 'x' 'z']

  , testCase "union, unordered" $ do
      let rs = union (rangeSet 'x' 'z') (rangeSet 'a' 'c')
      ranges rs @?= [Range.range 'a' 'c', Range.range 'x' 'z']

  , testCase "union, overlap" $ do
      let rs = union (rangeSet 'x' 'z') (rangeSet 'w' 'y')
      ranges rs @?= [Range.range 'w' 'z']

  , testCase "unions" $ do
      let rs = unions [ rangeSet 'c' 'f'
                      , rangeSet 'a' 'g'
                      , rangeSet 'd' 'j'
                      , rangeSet 'a' 'c'
                      , rangeSet 'm' 'p'
                      , rangeSet 'l' 's'
                      ]
      ranges rs @?= [Range.range 'a' 'j', Range.range 'l' 's']

  , testCase "union, overlap boundary" $ do
      let rs = union (rangeSet 'x' 'z') (rangeSet 'u' 'w')
      ranges rs @?= [Range.range 'u' 'z']

  , testCase "union, boundary edge case" $ do
      let rs = union (rangeSet '\x00' '\x10FFFF') (rangeSet '\x00' '\x10FFFF')
      ranges rs @?= [Range.range '\x00' '\x10FFFF']

  , testCase "union, singles" $ do
      let rs = union (rangeSet 'a' 'a') (rangeSet 'b' 'b')
      ranges rs @?= [Range.range 'a' 'b']
  ]

tests :: TestTree
tests = testGroup "Data.RangeSet" [properties, charSetTests]

main :: IO ()
main = defaultMain tests
