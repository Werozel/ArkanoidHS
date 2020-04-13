{-# LANGUAGE RecordWildCards #-}

import System.Timeout
import Run
import Data.Maybe (isNothing)
import System.Exit
import Lib
import Constants
import Base
import Test

getTests :: IO [Test]
getTests = do
  beginState <- initState "Test" 25 LevelView
  return [ Test "Стабильность работы" beginState Run.tick 0 (testSeconds * fps) doNothing True gameNotCrashed ]


main :: IO ()
-- main = putStrLn "Тестовый набор еще не реализован"
main = do
  tests <- getTests
  res <- runAllTests tests
  if res
    then putStrLn "All test passed"
    else do 
      putStrLn "Some tests failed"
      exitWith (ExitFailure 10)


runAllTests :: [Test] -> IO Bool
runAllTests [] = return True
runAllTests (test@Test{..}:xs) = do 
  thisResult <- runTest test
  otherResult <- runAllTests xs
  return (thisResult && otherResult)

doNothing :: GameState -> IO GameState
doNothing = return

gameNotCrashed :: Test -> Bool
gameNotCrashed test@Test{..} = getRemainingBricksCount (grid state) >= 0

runTest :: Test -> IO Bool
runTest test@Test {..}
  | tickCount >= tickLimit = return (checkSuccess test)
  | otherwise = do
    newState <- tick 0 state
    finalState <- testTick state
--    if (not waitAll) and (checkSuccess test)
    if not waitAll && checkSuccess test
      then return True
      else runTest test {state = finalState, tickCount = tickCount + 1}



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
