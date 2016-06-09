{-# LANGUAGE TemplateHaskell #-}

module TransitionMatrixTest
( runTests
) where

import Control.Monad (forM)
import Data.Matrix (fromList, transpose, toLists, Matrix, ncols, getCol)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()

import TransitionMatrix

newtype TestMatrix a = TestMatrix { getMatrix :: Matrix a } deriving (Eq, Show)
instance (Arbitrary a, Eq a, Num a) => Arbitrary (TestMatrix a) where
  arbitrary = let helper = do
                    n <- choose (2, 5)
                    v <- vector (n*n)
                    return $ TestMatrix $ fromList n n $ map abs v
               in helper `suchThat` isValid

newtype TestTransitionMatrix = TestTransitionMatrix TransitionMatrix deriving (Show, Eq)
instance Arbitrary TestTransitionMatrix where
  arbitrary = do
    m <- arbitrary `suchThat` isValid
    norm <- choose (0.0, 1.0)
    return $ TestTransitionMatrix $ toTransitionMatrix norm $ getMatrix m


isValid :: (Eq a, Num a) => TestMatrix a -> Bool
isValid (TestMatrix m) = let nc = ncols m
                             validVect = any (/=0)
                          in all validVect $ forM [1..nc] $ \j -> getCol j m

closeTo :: (Ord a, Num a) => a -> a -> a -> Bool
closeTo tol x y = abs (x - y) < tol

prop_idempotenceToNLMatrix :: TestMatrix Double -> Bool
prop_idempotenceToNLMatrix (TestMatrix m) = m == fromNLMatrix (toNLMatrix m)

prop_normalization :: Positive Double -> TestMatrix Double -> Bool
prop_normalization (Positive norm) (TestMatrix m) =
    let m' = toTransitionMatrix (1.0 - norm) m
        cols = toLists $ transpose $ toMatrix m'
        norms = map sum cols
     in all (closeTo 1.0E-4 norm) norms

prop_sameNormalization :: TestTransitionMatrix -> Bool
prop_sameNormalization (TestTransitionMatrix m) =
    let cols = toLists $ transpose $ toMatrix m
        norms = map sum cols
     in all (closeTo 1.0E-4 $ head norms) norms

prop_estimatePAbs :: Positive Double -> TestMatrix Double -> Bool
prop_estimatePAbs (Positive norm) (TestMatrix m) =
    let pAbs = 1.0 - norm
        m'   = toTransitionMatrix pAbs m
        est  = estimatePAbs m'
     in closeTo 1.0E-4 est pAbs


return []
runTests :: IO Bool
runTests  = $quickCheckAll
