module TransitionMatrix
( makeMatrix
, randomMatrix
, randomMatrixNullDiag
, mkRandomTransitionMatrix
, mkRandomNullDiagTransitionMatrix
, mkBlockTransitionMatrix
, toTransitionMatrix
, toNLMatrix
, fromNLMatrix
, eig
, estimatePAbs
, TransitionMatrix (..)
, TransitionMatrixGenerator (..)
) where

import Data.Matrix
import qualified Data.Vector as V
import System.Random
import Control.Monad (forM)
import qualified Numeric.LinearAlgebra as NL

import MC

newtype TransitionMatrix = TransitionMatrix { toMatrix :: Matrix Double }
                           deriving (Show, Eq)

data TransitionMatrixGenerator =
    RandomMG Int Double |
    RandomNullDiagMG Int Double |
    BlockMG Int Double Double deriving (Eq, Ord, Show)

-- | Produce a transition matrix
makeMatrix :: TransitionMatrixGenerator -- ^ a generator
              -> MC TransitionMatrix    -- ^ a TransitionMatrix, in the MC monad
makeMatrix (RandomMG size absorptionProbability) = mkRandomTransitionMatrix size absorptionProbability
makeMatrix (RandomNullDiagMG size absorptionProbability) = mkRandomNullDiagTransitionMatrix size absorptionProbability
makeMatrix (BlockMG size absorptionProbability coupling) = mkBlockTransitionMatrix size absorptionProbability coupling

{- |
   Produce a random transition matrix
-}
mkRandomTransitionMatrix :: Int           -- ^ the matrix size
                         -> Double        -- ^ the absorption probability
                         -> MC TransitionMatrix
mkRandomTransitionMatrix size pAbs = do
    m <- randomMatrix size
    return $ toTransitionMatrix pAbs m


{- |
   Produce a random transition matrix with null elements on the diagonal
-}
mkRandomNullDiagTransitionMatrix :: Int           -- ^ the matrix size
                                 -> Double        -- ^ the absorption probability
                                 -> MC TransitionMatrix
mkRandomNullDiagTransitionMatrix size pAbs = do
    m <- randomMatrixNullDiag size
    return $ toTransitionMatrix pAbs m


{- |
   Produce a block transition matrix
-}
mkBlockTransitionMatrix :: Int           -- ^ the matrix size
                        -> Double        -- ^ the absorption probability
                        -> Double        -- ^ the coupling
                        -> MC TransitionMatrix
mkBlockTransitionMatrix size pAbs coupling = do
    let blockSize1 = size `div` 2
    let blockSize2 = size - blockSize1
    m1 <- randomMatrix blockSize1
    m4 <- randomMatrix blockSize2
    let fromCoupling = blockSize1 `div` 2
    let toCoupling = blockSize2 `div` 2
    let m2 = zero blockSize2 blockSize1
    let m3 = matrix blockSize1 blockSize2 (\(i,j) -> if j==fromCoupling && i==toCoupling then coupling else 0.0)
    let m = joinBlocks (m1, m2, m3, m4)
    return $ toTransitionMatrix pAbs m

-- | Return a random square matrix of given size, with elements in the [0,1[
--   range
randomMatrix :: Int                 -- ^ size of the matrix
             -> MC (Matrix Double)  -- ^ the matrix, in the MC monad because it's random
randomMatrix n = do
    lss <- forM [0..n-1] $ \ _ -> randomList n
    return $ fromLists lss


-- | Return a random square matrix of given size, with elements in the [0,1[
--   range and null diagonal elements
randomMatrixNullDiag :: Int                 -- ^ size of the matrix
                     -> MC (Matrix Double)  -- ^ the matrix, in the MC monad because it's random
randomMatrixNullDiag n = do
    lss <- forM [0..n-1] $ \ k -> do
        ls <- randomList (n-1)
        let (left, right) = splitAt k ls
        let ls' = left ++ [0.0] ++ right
        return ls'
    return $ fromLists lss


-- | Generate a random list of size n
randomList :: (Random a, Fractional a)
           => Int   -- ^ the list size
           -> MC [a]
randomList 0 = return []
randomList n = do
    x <- uniform
    xs <- randomList (n-1)
    return (x:xs)


--normalizeRow :: Double -> Int -> Matrix Double -> Matrix Double
--normalizeRow norm i m = mapRow (\ _ x -> x*norm/tot) i m
--    where tot = sum $ getRow i m

normalizeCol :: Double -> Int -> Matrix Double -> Matrix Double
normalizeCol norm i m = mapCol (\ _ x -> x*norm/tot) i m
    where tot = sum $ getCol i m


{- |
   Rescale the elements of the given matrix by multiplying by the
   absorption probability
-}
toTransitionMatrix :: Double            -- ^ the absorption probability
                   -> Matrix Double     -- ^ the input matrix
                   -> TransitionMatrix
toTransitionMatrix pAbs m = let nc   = ncols m
                                norm = 1.0 - pAbs
                             in TransitionMatrix $ foldr (normalizeCol norm) m [1..nc]

-- functions for conversion to/from Numeric.LinearAlgebra

toNLMatrix :: NL.Element a => Matrix a -> NL.Matrix a
toNLMatrix = NL.fromLists . toLists

fromNLMatrix :: NL.Element a => NL.Matrix a -> Matrix a
fromNLMatrix = fromLists . NL.toLists

eig :: (NL.Element a, NL.Field a) => Matrix a -> (V.Vector (NL.Complex Double), Matrix (NL.Complex Double))
eig m = let (vnl, mnl) = NL.eig $ toNLMatrix m
            v' = V.fromList $ NL.toList vnl
            m' = fromNLMatrix mnl
         in (v', m')

avg :: (Num a, Fractional a, Foldable t) => t a -> a
avg v = sum v / fromIntegral (length v)

estimatePAbs :: TransitionMatrix -> Double
estimatePAbs (TransitionMatrix m) = 1.0 - fromIntegral (nrows m) * avg (toList m)
