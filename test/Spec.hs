import MCTest
import TransitionMatrixTest

main :: IO ()
main = do
    _ <- MCTest.runTests
    _ <- TransitionMatrixTest.runTests
    return ()
