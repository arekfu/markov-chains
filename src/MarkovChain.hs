module MarkovChain
( runMarkovChain
, step
, stepUntilAbsorption
, SystemState
, MarkovChain
) where

import Control.Monad.Reader
import Data.Matrix
import TransitionMatrix

import MC

type SystemState = Int

type MarkovChain = ReaderT TransitionMatrix MC

-- | Take one step: transition from a state to another
step :: SystemState -> MarkovChain (Maybe SystemState)
step state = do
    matrix_ <- ask
    let probs = getCol state $ toMatrix matrix_
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

runMarkovChain :: ReaderT r m a -> r -> m a
runMarkovChain = runReaderT
