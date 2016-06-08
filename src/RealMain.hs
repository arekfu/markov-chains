{-
 - A simple program to test some ideas about the determination of the adjoint
 - flux in MC
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module RealMain
( realMain
) where

import Control.Monad (replicateM, when, forM_)
import System.Random
import System.Console.CmdArgs

import MC
import MarkovChain
import Scores
import TransitionMatrix

process :: Options -> IO ()
process (Options shots_ seed_ dim pabs coup tmTyp) = do
    let mg = case tmTyp of
                 "random"           -> RandomMG dim pabs
                 "random-null-diag" -> RandomNullDiagMG dim pabs
                 "block"            -> BlockMG dim pabs coup
                 s                  -> error $ "Unrecognized algorithm: " ++ s
    let gen = mkStdGen seed_
    let matrixAct = makeMatrix mg
    let (m, gen') = runMC matrixAct gen
    putStrLn "Generated transition matrix:"
    print m
    let (steps, _) = runMC (simulateNChains shots_ 1 m) gen'
    when (shots_<100) $ do
        putStrLn "Generated Markov chains:"
        print steps
    let scores = [scoreAdjoint, scoreEigensystem, scoreFlux, scoreChainLength]
    forM_ scores $ \score -> wrapScore score steps shots_ m

simulateNChains :: Int -> SystemState -> TransitionMatrix -> MC [[SystemState]]
simulateNChains shots_ initialState matrix =
    replicateM shots_ $ runMarkovChain (stepUntilAbsorption initialState) matrix

--------------
-- CLI options

data Options = Options { shots :: Int       -- ^ number of shots
                       , seed :: Int        -- ^ MC seed
                       , dimension :: Int   -- ^ dimension of the system space
                       , absorptionProbability :: Double    -- ^ absorption probability per step
                       , coupling :: Double                 -- ^ coupling coefficient
                       , tmType :: String                   -- ^ type of transition matrix
                       } deriving (Show, Data, Typeable)

defaultOptions :: Options
defaultOptions = Options
    { shots = 10
      &= name "n"
      &= help "number of Monte-Carlo shots"
      &= typ "SHOTS"
    , seed = 12345
      &= name "s"
      &= help "Monte-Carlo seed"
      &= typ "SEED"
    , dimension = 15
      &= name "d"
      &= help "dimension of the state space"
      &= typ "DIM"
    , absorptionProbability = 0.1
      &= explicit &= name "a" &= name "absorption-probability"
      &= help "absorption probability"
      &= typ "PABS"
    , coupling = 0.2
      &= name "c"
      &= help "coupling constant for the block matrix generator"
      &= typ "COUPLING"
    , tmType = "random"
      &= explicit &= name "t" &= name "type"
      &= help "algorithm to generate the transition matrix; must be one of \
              \\"random\", \"random-null-diag\", \"block\""
      &= typ "ALGORITHM"
    } &= program "markov-chains"
      &= summary "markov-chains v0.1.0.0"
      &= helpArg [explicit, name "h", name "help"]

realMain :: IO ()
realMain = do
    args_ <- cmdArgs defaultOptions
    process args_
