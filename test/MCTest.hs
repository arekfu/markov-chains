module MCTest
( allTests
) where

import qualified Data.Vector as V
import Control.Monad (liftM)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (vector)
import Data.Maybe (isNothing)

import MC

allPositive :: (Ord a, Num a) => V.Vector a -> Bool
allPositive = V.all (>0)

newtype PositiveVector a = PositiveVector (V.Vector a)
                           deriving (Show, Eq, Ord)

fromVector :: (Ord a, Num a, Fractional a) => V.Vector a -> PositiveVector a
fromVector v = let tot = V.sum v
                in PositiveVector $ fmap (/tot) v

getVector (PositiveVector v) = v

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = do
    n <- choose (1, 20)
    v <- vector n
    return $ V.fromList v

instance (Arbitrary a, Ord a, Num a, Fractional a) => Arbitrary (PositiveVector a) where
  arbitrary = do
    v <- arbitrary `suchThat` allPositive
    return $ fromVector v

prop_getFirst :: PositiveVector Double -> Bool
prop_getFirst (PositiveVector v) = let i = sampleV v 0.0
                                    in i == Just 1

prop_getLast :: PositiveVector Double -> Bool
prop_getLast (PositiveVector v) = let i = sampleV v 1.1
                                   in isNothing i

allTests = [ prop_getFirst
           , prop_getLast
           ]
