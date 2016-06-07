import Test.QuickCheck
import Control.Monad (forM_)
import MCTest

tests = concat [ MCTest.allTests
               ]
main = forM_ allTests $ \prop -> quickCheck prop
