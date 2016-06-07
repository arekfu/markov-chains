import Test.QuickCheck
import Control.Monad (forM_)

import MCTest
import TransitionMatrixTest

main = do
    MCTest.runTests
    TransitionMatrixTest.runTests
