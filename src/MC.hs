module MC
( MC
, Seed
, runMC
, uniform
, sampleV
, sampleUniformV
, getGen
) where

import System.Random
import Control.Monad.State
import qualified Data.Vector as V

type Seed = Int

type MC = State StdGen

getGen :: MC StdGen
getGen = get

runMC = runState

uniform :: (Random a, Fractional a) => MC a
uniform = do
    gen <- getGen
    let (xi, gen') = randomR (0.0, 1.0) gen
    put gen'
    return xi

sampleV :: (Num a, Ord a, Fractional a, Random a)
        => V.Vector a
        -> a
        -> Int
sampleV v xi =
    let v' = V.scanl1' (+) v
        i  = V.findIndex (>xi) v'
     in case i of
          Nothing -> length v
          Just j  -> j+1

sampleUniformV :: (Num a, Ord a, Fractional a, Random a)
        => V.Vector a
        -> MC Int
sampleUniformV v = do
    xi <- uniform
    return $ sampleV v xi