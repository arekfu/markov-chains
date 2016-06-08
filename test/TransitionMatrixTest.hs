{-# LANGUAGE TemplateHaskell #-}

module TransitionMatrixTest
( runTests
) where

import Control.Monad (forM)
import Data.List (all)
import Data.Matrix (fromList, transpose, toLists, Matrix, nrows, ncols, zero, getCol)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (vector)
import Test.QuickCheck.Modifiers (Positive)

import TransitionMatrix

isValid :: (Eq a, Num a) => Matrix a -> Bool
isValid m = let nc = ncols m
                validVect = any (/=0)
             in all validVect $ forM [1..nc] $ \j -> getCol j m

instance (Arbitrary a, Eq a, Num a) => Arbitrary (Matrix a) where
  arbitrary = let helper = do
                    n <- choose (2, 5)
                    v <- vector (n*n)
                    return $ fromList n n $ map abs v
               in helper `suchThat` isValid

instance Arbitrary TransitionMatrix where
  arbitrary = do
    m <- arbitrary `suchThat` isValid
    norm <- choose (0.0, 1.0)
    return $ toTransitionMatrix norm m


closeTo :: (Ord a, Num a) => a -> a -> a -> Bool
closeTo tol x y = abs (x - y) < tol

prop_idempotenceToNLMatrix :: Matrix Double -> Bool
prop_idempotenceToNLMatrix m = m == fromNLMatrix (toNLMatrix m)

prop_normalization :: Positive Double -> Matrix Double -> Bool
prop_normalization (Positive norm) m = let m' = toTransitionMatrix (1.0 - norm) m
                                           cols = toLists $ transpose $ toMatrix m'
                                           norms = map sum cols
                                        in all (closeTo 1.0E-4 norm) norms

prop_sameNormalization :: TransitionMatrix -> Bool
prop_sameNormalization m = let cols = toLists $ transpose $ toMatrix m
                               norms = map sum cols
                            in all (closeTo 1.0E-4 $ head norms) norms
return []
runTests  = $quickCheckAll
