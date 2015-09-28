{-
 - A simple program to test some ideas about the determination of the adjoint flux in MC
 -}

import Data.Matrix
import Control.Monad
import System.Random
import Data.List (foldl')

size = 15
absorptionProbability = 0.1

-- | Return a random square matrix of given size, with elements in the [0,1[ range
randomMatrix :: Int                 -- ^ size of the matrix
             -> IO (Matrix Double)  -- ^ the matrix, in the IO monad because it's random
randomMatrix n = do
    ls <- forM [1..n] $ \ _ -> forM [1..n] $ \ _ -> randomIO
    return $ fromLists ls


normalizeRow :: Double -> Matrix Double -> Int -> Matrix Double
normalizeRow norm m i = mapRow (\ _ x -> x*norm/tot) i m
    where tot = sum $ getRow i m


{- |
   Adjust the elements of the given matrix so that it can be used as a transition matrix
   (with the understanding that W_{i,j} is the probability for the i -> j transition)
-}
toTransitionMatrix :: Double        -- ^ the absorption probability
                   -> Matrix Double -- ^ the input matrix
                   -> Matrix Double
toTransitionMatrix pAbs m = extended  -- <-> fromAbsorption
    where norm = 1.0 - pAbs
          normalized = foldl' (normalizeRow norm) m [1..nrows m]
          extended = setSize pAbs (nrows m) (ncols m + 1) normalized
          fromAbsorption = rowVector 

main = do
    m <- randomMatrix (size-1)
    print m
