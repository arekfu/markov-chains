module TransitionMatrixGenerator
( makeMatrix
, mkRandomTransitionMatrix
, mkBlockTransitionMatrix
, TransitionMatrixGenerator (..)
) where

import Data.Matrix
import MarkovChain
import MC

data TransitionMatrixGenerator =
    RandomMG Int Double |
    BlockMG Int Double Double deriving (Eq, Ord, Show)

-- | Produce a transition matrix
makeMatrix :: TransitionMatrixGenerator -- ^ a generator
              -> MC TransitionMatrix    -- ^ a TransitionMatrix, in the MC monad
makeMatrix (RandomMG size absorptionProbability) = mkRandomTransitionMatrix size absorptionProbability
makeMatrix (BlockMG size absorptionProbability coupling) = mkBlockTransitionMatrix size absorptionProbability coupling

{- |
   Produce a random transition matrix
-}
mkRandomTransitionMatrix :: Int           -- ^ the matrix size
                         -> Double        -- ^ the absorption probability
                         -> MC TransitionMatrix
mkRandomTransitionMatrix size pAbs = do
    m <- randomMatrix (size-1)
    return $ toTransitionMatrix pAbs m


{- |
   Produce a block transition matrix
-}
mkBlockTransitionMatrix :: Int           -- ^ the matrix size
                        -> Double        -- ^ the absorption probability
                        -> Double        -- ^ the coupling
                        -> MC TransitionMatrix
mkBlockTransitionMatrix size pAbs coupling = do
    let blockSize1 = (size-1) `div` 2
    let blockSize2 = (size-1) - blockSize1
    m1 <- randomMatrix blockSize1
    m4 <- randomMatrix blockSize2
    let fromCoupling = blockSize1 `div` 2
    let toCoupling = blockSize1 + (blockSize2 `div` 2)
    let m2 = matrix blockSize1 blockSize2 (\(i,j) -> if i==fromCoupling && j==toCoupling then coupling else 0.0)
    let m3 = zero blockSize2 blockSize1
    let m = joinBlocks (m1, m2, m3, m4)
    return $ toTransitionMatrix pAbs m
