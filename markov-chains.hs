{-
 - A simple program to test some ideas about the determination of the adjoint flux in MC
 -}

import Control.Monad (replicateM)
import System.Random
import System.Environment (getArgs)

import MC
import MarkovChain



process :: Int -> Double -> Int -> Seed -> IO ()
process size absorptionProbability shots seed = do
    let gen = mkStdGen seed
    let (m, gen') = runMC (mkTransitionMatrix size absorptionProbability) gen
    putStrLn "Generated transition matrix:"
    print m
    let (steps, gen'') = runMC (simulateNChains shots 1 m) gen'
    putStrLn "Generated Markov chains:"
    print steps

simulateNChains :: Int -> SystemState -> TransitionMatrix -> MC [[SystemState]]
simulateNChains shots initialState matrix = do
    chains <- replicateM shots $ runMarkovChain stepUntilAbsorption matrix initialState
    return $ map fst chains

defaultSize = 15
defaultAbsorptionProbability = 0.1
defaultSeed = 12345
defaultShots = 10

main = do
    args <- getArgs
    let size = if length args > 0 then read $ head args else defaultSize
    let absorptionProbability = if length args > 1 then read $ args !! 1 else defaultAbsorptionProbability
    let shots = if length args > 2 then read $ args !! 2 else defaultShots
    let seed = if length args > 3 then read $ args !! 3 else defaultSeed
    process size absorptionProbability shots seed
