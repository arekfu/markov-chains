{-
 - A simple program to test some ideas about the determination of the adjoint flux in MC
 -}

import Control.Monad (replicateM)
import System.Random
import System.Environment (getArgs)

import MC
import MarkovChain



type Seed = Int

process :: Int -> Double -> Seed -> IO ()
process size absorptionProbability seed = do
    let gen = mkStdGen seed
    let (m, gen') = runMC (mkTransitionMatrix size absorptionProbability) gen
    let (steps, gen'') = runMC (runMarkovChain (step 1) m) gen'
    print steps


defaultSize = 15
defaultAbsorptionProbability = 0.1
defaultSeed = 12345

main = do
    args <- getArgs
    let size = if length args > 0 then read $ head args else defaultSize
    let absorptionProbability = if length args > 1 then read $ args !! 1 else defaultAbsorptionProbability
    let seed = if length args > 2 then read $ args !! 2 else defaultSeed
    process size absorptionProbability seed
