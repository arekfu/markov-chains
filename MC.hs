module MC
( MC
, get
, put
, runMC
) where

import System.Random
import Control.Monad.State


type MC = State StdGen

runMC = runState
