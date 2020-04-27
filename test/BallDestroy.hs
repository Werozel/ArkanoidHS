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
  let speed = ballSpeed / fromIntegral fps
  setPosition
    beginState {
        ballPos = (fst blockPosition, snd blockPosition - Constants.brickHeight - speed - Constants.ballRadius),
        ballDirection = (0, speed)
      }

-- Returns first brick from the grid
getFirstBlock :: GameState -> Brick
getFirstBlock state@GameState{..} = head (head (bricks grid))


-- Set ball towards first brick if ball is moving downwards
setPosition :: GameState -> IO GameState
setPosition state@GameState {..} = do
  let brick = getFirstBlock state
  let blockPosition = position brick
  let speed = ballSpeed / fromIntegral fps
  if brick == NoBrick
    then return state
    else
      if snd ballDirection < 0
        then return
               state
                 { ballPos = (fst blockPosition, snd blockPosition - Constants.brickHeight / 2 - speed - Constants.ballRadius)
                 , ballDirection = (0, speed)
                 , grid = grid{lastHit = NoHit}
                 }
        else return state

-- Checks if first block is destroyed
checkBlockDestroyed :: Test -> Bool
checkBlockDestroyed test@Test {..} = getFirstBlock state == NoBrick

