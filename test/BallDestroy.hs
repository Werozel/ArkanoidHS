{-# LANGUAGE RecordWildCards #-}

module BallDestroy where

import Graphics.Gloss.Interface.Pure.Game
import Lib
import Constants
import Test
import Run

getBallDestroyBeginState :: IO GameState
getBallDestroyBeginState = do
  beginState <- initState "Test" 25 LevelView
  let blockPosition = position (getFirstBlock beginState)
  setPosition
    beginState {
        ballPos = (fst blockPosition + Constants.brickLength / 2, snd blockPosition - Constants.brickHeight / 2),
        ballDirection = (0, ballSpeed)
      }

-- Returns first brick from the grid
getFirstBlock :: GameState -> Brick
getFirstBlock state@GameState{..} = head (head (bricks grid))


-- Set ball towards first brick if ball is moving downwards
setPosition :: GameState -> IO GameState
setPosition state@GameState {..} = do
  let blockPosition = position (getFirstBlock state)
--  print (hitsLeft (getFirstBlock state))
--  print (show ballDirection ++ " " ++ show ballPos ++ " " ++ show blockPosition)
  if snd ballDirection < 0
    then return
           state
             { ballPos = (fst blockPosition + Constants.brickLength / 2, snd blockPosition - Constants.brickHeight / 2)
             , ballDirection = (0, ballSpeed)
             }
    else return state
-- Checks if first block is destroyed
checkBlockDestroyed :: Test -> Bool
checkBlockDestroyed test@Test{..} = hitsLeft (getFirstBlock state) <= 0

