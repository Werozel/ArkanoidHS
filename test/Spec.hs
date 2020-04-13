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
main = do
  initState <- initState "Test" 25 LevelView
  res <- gameTest initState tick (testSeconds * fps)
  if getRemainingBricksCount (grid res) >= 0
    then putStrLn "Test passed"
    else exitWith (ExitFailure 10)


-- Функция, вызывающая время тиковых шагов
gameTest :: GameState -> (Float -> GameState -> IO GameState) -> Int -> IO GameState
gameTest state@GameState{..} tick 0 = return state
gameTest state@GameState {..} tick steps = do
  newState <- tick 0 state
  gameTest newState tick (steps - 1)


-- Стабильность работы (готово)
-- Уничтожение блока
-- Проигрыш
-- Конец игры (выигрыш)
-- Сохранение + загрузка
