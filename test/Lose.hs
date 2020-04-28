{-# LANGUAGE RecordWildCards #-}

module Lose where

import Lib
import Test

awayFromTheBall :: GameState -> IO GameState
awayFromTheBall state@GameState{..} = return state {
  platformPos = (- fst ballPos, snd platformPos)
}

checkLose :: Test -> Bool
checkLose test@Test {..} = result state == Lose
