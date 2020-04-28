{-# LANGUAGE RecordWildCards #-}

module SaveTest where

import Data.Time.Clock
import Data.String

import Lib
import Test
import Save
import Run
import Constants
import TestConstants
import ResultService

saveTestInitState :: IO GameState
saveTestInitState = initState saveTestName 25 LevelView


saveWhenTimeComes :: GameState -> IO GameState
saveWhenTimeComes state@GameState {..} = do
  saveResult state {playTime = secondsToNominalDiffTime (realToFrac saveTestNumber)}
  load <- getResultText resultsFilePath
  let lastResult = head $ tail $ lines $ reverse (takeLast 10 $ splitResults load) !! 1
  return state{result = if lastResult == saveTestName ++ " " ++ show saveTestNumber ++ "s" then TestSuccess else Lose}

checkCompleteSaveTest :: Test -> Bool
checkCompleteSaveTest test@Test{..} = result state == TestSuccess
