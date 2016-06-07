module MarkovChain
( runMarkovChain
, randomMatrix
, randomMatrixNullDiag
, toTransitionMatrix
, step
, stepUntilAbsorption
, TransitionMatrix
, SystemState
, MarkovChain
) where

import Control.Monad (when)
import Control.Monad.Reader
import Data.Matrix
import qualified Data.Vector as V
import Data.List (unfoldr)
import System.Random

import MC

type TransitionMatrix = Matrix Double
type SystemState = Int

type MarkovChain = ReaderT TransitionMatrix MC

-- | Return a random square matrix of given size, with elements in the [0,1[
--   range and null diagonal elements
randomMatrix :: Int                 -- ^ size of the matrix
             -> MC TransitionMatrix -- ^ the matrix, in the MC monad because it's random
randomMatrix n = do
    lss <- forM [0..n-1] $ \ k -> randomList n
    return $ fromLists lss


-- | Return a random square matrix of given size, with elements in the [0,1[
--   range and null diagonal elements
randomMatrixNullDiag :: Int         -- ^ size of the matrix
             -> MC TransitionMatrix -- ^ the matrix, in the MC monad because it's random
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


normalizeRow :: Double -> TransitionMatrix -> Int -> TransitionMatrix
normalizeRow norm m i = mapRow (\ _ x -> x*norm/tot) i m
    where tot = sum $ getRow i m


{- |
   Rescale the elements of the given matrix by multiplying by the
   absorption probability
-}
toTransitionMatrix :: Double            -- ^ the absorption probability
                   -> TransitionMatrix  -- ^ the input matrix
                   -> TransitionMatrix
toTransitionMatrix = scaleMatrix


-- | Take one step: transition from a state to another
step :: SystemState -> MarkovChain (Maybe SystemState)
step state = do
    matrix <- ask
    let probs = getRow state matrix
    lift $ sampleUniformV probs

actWhileM :: Monad m => a -> (a -> m (Maybe a)) -> m [a]
actWhileM initialState act = do
    newState <- act initialState
    case newState of
      Nothing -> return []
      Just s  -> do
        rest <- actWhileM s act
        return (s:rest)

-- | Take steps until we hit absorption
stepUntilAbsorption :: SystemState -> MarkovChain [SystemState]
stepUntilAbsorption initialState = actWhileM initialState step

runMarkovChain = runReaderT
