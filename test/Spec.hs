{-# LANGUAGE RecordWildCards #-}

import System.Timeout
import Run
import Data.Maybe (isNothing)
import System.Exit
import Lib
import Constants
import Base

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
main
  | getRemainingBricksCount (grid res) >= 0 = putStrLn "Test passed"
  | otherwise = exitWith (ExitFailure 10)
  where
    res = gameTest (initState 0 LevelView) tick (testSeconds * fps)

-- Function that calls tick steps times
gameTest :: GameState -> (Float -> GameState -> GameState) -> Int -> GameState
gameTest state@GameState{..} tick 0 = state
gameTest state@GameState{..} tick steps = gameTest (tick 0 state) tick (steps - 1)

