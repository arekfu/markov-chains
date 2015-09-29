module MarkovChain
( runMarkovChain
, mkTransitionMatrix
, step
, TransitionMatrix
, SystemState
, MarkovChain
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Matrix
import qualified Data.Vector as V
import Data.List (foldl')
import System.Random
import qualified Debug.Trace as D

import MC

type TransitionMatrix = Matrix Double
type SystemState = Int

type MarkovChain = ReaderT TransitionMatrix (StateT SystemState MC)

-- | Return a random square matrix of given size, with elements in the [0,1[ range
randomMatrix :: Int                 -- ^ size of the matrix
             -> MC TransitionMatrix  -- ^ the matrix, in the MC monad because it's random
randomMatrix n = do
    lss <- forM [1..n] $ \ _ -> do
                ls <- randomList n
                return ls
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


normalizeRow :: Double -> TransitionMatrix -> Int -> TransitionMatrix
normalizeRow norm m i = mapRow (\ _ x -> x*norm/tot) i m
    where tot = sum $ getRow i m


{- |
   Adjust the elements of the given matrix so that it can be used as a transition matrix
   (with the understanding that W_{i,j} is the probability for the i -> j transition)
-}
toTransitionMatrix :: Double        -- ^ the absorption probability
                   -> TransitionMatrix -- ^ the input matrix
                   -> TransitionMatrix
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
                   -> MC TransitionMatrix
mkTransitionMatrix size pAbs = do
    m <- randomMatrix (size-1)
    return $ toTransitionMatrix pAbs m


-- | Take one step: transition from a state to another
step :: MarkovChain SystemState
step = do
    matrix <- ask
    state <- get
    let probs = getRow state matrix
    i <- lift $ lift $ sampleV probs
    put i
    return i

getState :: MarkovChain SystemState
getState = get

runMarkovChain action matrix initialState = runStateT (runReaderT action matrix) initialState
