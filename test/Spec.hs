
import System.Timeout
import Run
import Data.Maybe (isNothing)
import System.Exit

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
main = do
  res <- timeout 5000 run
  if isNothing res
    then putStrLn "Game launched"
    else exitWith (ExitFailure 1)


