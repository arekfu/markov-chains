module Scores
( wrapScore
, scoreAdjoint
, scoreEigensystem
, scoreFlux
, scoreChainLength
) where

import Data.List.Ordered (sort, nub)
import Data.Matrix (nrows, identity)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)

import MarkovChain
import TransitionMatrix

type Score = [[SystemState]] -> Int -> TransitionMatrix -> IO ()

wrapScore :: Score -> Score
wrapScore score chains shots matrix = do
    putStrLn "*************************************"
    score chains shots matrix
    putStrLn ""

scoreAdjoint :: Score
scoreAdjoint chains shots matrix = do
    let nStates = nrows $ toMatrix matrix
    let detectorState = nStates - 1
    let vec = runST $ do
         v <- VM.new nStates
         VM.set v (0.0 :: Double)
         forM_ chains $ \ chain ->
             when (detectorState `elem` chain) $ do
                 let states = nub $ sort chain
                 forM_ states $ \ state ->
                     VM.modify v succ (state-1)
         forM_ [0..nStates-1] $ \k -> VM.modify v (/ fromIntegral shots) k
         V.freeze v
    putStrLn "Estimated adjoint:"
    print vec

scoreEigensystem :: Score
scoreEigensystem _ _ m = do
    putStrLn "Eigensystem:"
    let m' = toMatrix m
    let mMinus1 = m' - identity (nrows m')
    print $ eig mMinus1

scoreFlux :: Score
scoreFlux chains shots matrix = do
    let nStates = nrows $ toMatrix matrix
    let vec = runST $ do
         v <- VM.new nStates
         VM.set v (0.0 :: Double)
         forM_ chains $ \ chain ->
            forM_ chain $ \ state ->
                VM.modify v succ (state-1)
         forM_ [0..nStates-1] $ \k -> VM.modify v (/ fromIntegral shots) k
         V.freeze v
    putStrLn "Estimated flux:"
    print vec

scoreChainLength :: Score
scoreChainLength chains shots mat = do
    let lengths = map length chains
    let averageLength = fromIntegral (sum lengths) / fromIntegral shots :: Double
    let pAbs = estimatePAbs mat
    let expectedLength = (1.0-pAbs)/pAbs
    putStrLn "Average length (expected):"
    putStrLn (show averageLength ++ " (" ++ show expectedLength ++ ")")
