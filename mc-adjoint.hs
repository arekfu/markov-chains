{-
 - A simple program to test some ideas about the determination of the adjoint flux in MC
 -}

import Data.Matrix
import qualified Data.Vector as V
import Control.Monad (forM)
import System.Random
import Data.List (foldl')
import System.Environment (getArgs)

import MC

-- | Return a random square matrix of given size, with elements in the [0,1[ range
randomMatrixIO :: Int                 -- ^ size of the matrix
               -> IO (Matrix Double)  -- ^ the matrix, in the IO monad because it's random
randomMatrixIO n = do
    ls <- forM [1..n] $ \ _ -> forM [1..n] $ \ _ -> randomIO
    return $ fromLists ls

-- | Return a random square matrix of given size, with elements in the [0,1[ range
randomMatrix :: Int                 -- ^ size of the matrix
             -> MC (Matrix Double)  -- ^ the matrix, in the MC monad because it's random
randomMatrix n = do
    lss <- forM [1..n] $ \ _ -> do
                ls <- randomList n
                return ls
    return $ fromLists lss


-- | Generate a random list of size n
randomList :: Random a
           => Int   -- ^ the list size
           -> MC [a]
randomList 0 = return []
randomList n = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    xs <- randomList (n-1)
    return (x:xs)


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
toTransitionMatrix pAbs m = extended  <-> fromAbsorption
    where rs = nrows m
          cs = ncols m
          norm = 1.0 - pAbs
          normalized = foldl' (normalizeRow norm) m [1..rs]
          extended = setSize pAbs rs (cs + 1) normalized
          fromAbsorption = rowVector $ (V.replicate cs 0.0) `V.snoc` 1.0


{- |
   Produce a transition matrix
-}
mkTransitionMatrix :: Int           -- ^ the matrix size
                   -> Double        -- ^ the absorption probability
                   -> MC (Matrix Double)
mkTransitionMatrix size pAbs = do
    m <- randomMatrix (size-1)
    return $ toTransitionMatrix pAbs m


process size absorptionProbability seed = do
    let gen = mkStdGen seed
    let (m, gen') = runMC (mkTransitionMatrix size absorptionProbability) gen
    print m


defaultSize = 15
defaultAbsorptionProbability = 0.1
defaultSeed = 12345

main = do
    args <- getArgs
    let size = if length args > 0 then read $ head args else defaultSize
    let absorptionProbability = if length args > 1 then read $ args !! 1 else defaultAbsorptionProbability
    let seed = if length args > 2 then read $ args !! 2 else defaultSeed
    process size absorptionProbability seed
