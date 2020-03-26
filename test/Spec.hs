{-# LANGUAGE RecordWildCards #-}

import System.Timeout
import Run
import Data.Maybe (isNothing)
import System.Exit
import Lib
import Constants
import Base

main :: IO ()
-- main = putStrLn "Тестовый набор еще не реализован"
main
  | getRemainingBricksCount (grid res) >= 0 = putStrLn "Test passed"
  | otherwise = exitWith (ExitFailure 10)
  where
    res = gameTest (initState 25 LevelView) tick (testSeconds * fps)

-- Функция, вызывающая время тиковых шагов
gameTest :: GameState -> (Float -> GameState -> GameState) -> Int -> GameState
gameTest state@GameState{..} tick 0 = state
gameTest state@GameState{..} tick steps = gameTest (tick 0 state) tick (steps - 1)
