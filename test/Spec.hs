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

import BallDestroy

getTests :: IO [Test]
getTests = do
  beginState <- initState "Test" 25 LevelView
  ballDestroyBeginState <- getBallDestroyBeginState
  return [ Test "Overall stability" beginState Run.tick 0 (testSeconds * fps) doNothing True gameNotCrashed,
    Test "Brick destroy" ballDestroyBeginState Run.tick 0 (testSeconds * fps) setPosition False checkBlockDestroyed,
    Test "Lose the game" beginState Run.tick 0 (testSeconds * fps) awayFromTheBall False checkLose,
    Test "Win the game" beginState Run.tick 0 (testSeconds * fps * 200) followFromTheBall False checkWin ]


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
runAllTests (test@Test {..}:xs) = do
  print ("Started " ++ name)
  thisResult <- runTest test
  otherResult <- runAllTests xs
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
--    print ("Before " ++ show (ballDirection state) ++ " " ++ show (ballPos state) ++ " " ++ show (result state))
    newState <- tick 0 state
--    print ("Tick passed " ++ show (ballDirection newState) ++ " " ++ show (ballPos newState) ++ " " ++ show (lastHit (grid state)))
    finalState <- testTick newState
--    print ("TestTick passed " ++ show (ballDirection finalState) ++ " " ++ show (ballPos finalState) ++ " ")
--    if (not waitAll) and (checkSuccess test)
    if not waitAll && checkSuccess test
      then return True
      else runTest test {state = finalState, tickCount = tickCount + 1}


-- Стабильность работы (готово)
-- Уничтожение блока (готово)
-- Проигрыш (готово)
-- Конец игры (выигрыш)
