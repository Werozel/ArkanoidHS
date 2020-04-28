{-# LANGUAGE RecordWildCards #-}

import System.Timeout
import Run
import Data.Maybe (isNothing)
import System.Exit
import Lib
import Constants
import Base
import Test
import Lose
import Win
import SaveTest

import BallDestroy

getTests :: IO [Test]
getTests = do
  beginState <- initState "TeSt" 25 LevelView
  saveTestState <- saveTestInitState
  ballDestroyBeginState <- getBallDestroyBeginState
  return [ Test "Overall stability" beginState Run.tick 0 (testSeconds * fps) doNothing True gameNotCrashed,
    Test "Brick destroy" ballDestroyBeginState Run.tick 0 (testSeconds * fps) setPosition False checkBlockDestroyed,
    Test "Lose the game" beginState Run.tick 0 (testSeconds * fps) awayFromTheBall False checkLose,
    Test "Win the game" beginState Run.tick 0 (testSeconds * fps * 200) followFromTheBall False checkWin,
    Test "Save and Load" saveTestState Run.tick 0 (testSeconds * fps) saveWhenTimeComes False checkCompleteSaveTest]


main :: IO ()
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
runAllTests (test@Test {..}:xs) = do
  thisResult <- runTest test
  otherResult <- runAllTests xs
  print ("Started " ++ name)
  if not thisResult
    then putStrLn ("Failed " ++ name)
    else putStrLn ("Passed " ++ name)
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
    finalState <- testTick newState
    if not waitAll && checkSuccess test
      then return True
      else runTest test {state = finalState, tickCount = tickCount + 1}


-- Стабильность работы (готово)
-- Уничтожение блока (готово)
-- Проигрыш (готово)
-- Конец игры (выигрыш) (готово)
-- Сохранение и загрузка статистики (готово)
