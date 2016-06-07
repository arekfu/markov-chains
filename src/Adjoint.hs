module Adjoint
( estimateAdjoint
) where

import Data.List.Ordered (sort, nub)
import Data.Matrix (nrows)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)

import MC
import MarkovChain
import TransitionMatrix

estimateAdjoint :: [[SystemState]] -> Int -> TransitionMatrix -> V.Vector Double
estimateAdjoint chains shots matrix =
    let nStates = nrows $ toMatrix matrix
        detectorState = nStates - 1
    in runST $ do
        v <- VM.new nStates
        VM.set v 0.0
        forM_ chains $ \ chain ->
            when (detectorState `elem` chain) $ do
                let states = nub $ sort chain
                forM_ states $ \ state ->
                    VM.modify v succ (state-1)
        forM_ [0..nStates-1] $ \k -> VM.modify v (/ fromIntegral shots) k
        V.freeze v
