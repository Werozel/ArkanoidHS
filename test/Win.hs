{-# LANGUAGE RecordWildCards #-}

module Win where

import System.Random

import Lib
import Test
import Base

randomPlatformOffsetRange :: (Int, Int)
randomPlatformOffsetRange = (-40, 40)

followFromTheBall :: GameState -> IO GameState
followFromTheBall state@GameState{..} = do
  gen <- newStdGen
  let offset = fst (randomR randomPlatformOffsetRange gen)
  return state {
  platformPos = (fst ballPos + fromIntegral offset , snd platformPos)
}

checkWin :: Test -> Bool
checkWin test@Test {..} = result state == Win
