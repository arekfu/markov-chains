module MC
( MC
, get
, put
, runMC
, uniform
, sampleV
) where

import System.Random
import Control.Monad.State
import qualified Data.Vector as V


type MC = State StdGen

runMC = runState

uniform :: (Random a, Fractional a) => MC a
uniform = do
    gen <- get
    let (xi, gen') = randomR (0.0, 1.0) gen
    put gen'
    return xi

sampleV :: (Num a, Ord a, Fractional a, Random a, Show a)
        => V.Vector a
        -> MC Int
sampleV v = do
    xi <- uniform
    let v' = V.scanl1' (+) v
    let i = V.findIndex (>xi) v'
    return $ case i of
        Nothing -> length v
        Just j -> j+1
