{-# LANGUAGE RecordWildCards #-}
module Run
  ( run
  ) where

import Graphics.Gloss.Interface.Pure.Game

import System.Random

import Service
import Lib
import Constants
import Base
import LevelGenerator
import DrawFunctions


-- Returns initial game state
initState :: Float -> GameState
initState rnd = GameState False MainMenu initBallPos initBallDirection initPlatformPos 0 initGrid 3 NotFinished [NonePressed]
  where
    initBallPos :: Point
    initBallPos = (0, initBallPositionY)

    initBallDirection :: Point
    initBallDirection = (rnd / fromIntegral fps, ballVerticalDirection / fromIntegral fps)
      where
        ballVerticalDirection = sqrt ((ballSpeed * ballSpeed) - (rnd * rnd))

    initPlatformPos :: Point
    initPlatformPos = (0, initPlatformPositionY)
    
    initGrid = generateLevel 1


-- Changes game state with each tick
tick ::Float -> GameState -> GameState
tick _ state@GameState{..} | result == Win =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                           | result == Lose =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Lose [NonePressed]
                           | otherwise = GameState isPlaying view newBallPos newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
                            where
                              newBallPos = moveBall ballPos ballDirection
                              newGrid = detectHit newBallPos (bricks grid)
                              hit = lastHit newGrid
                              platformHitFlag = checkPlatformHit newBallPos state
                              resHit | platformHitFlag = PlatformHit
                                     | otherwise = hit
                              bricksLeftUpdated = getRemainingBricksCount newGrid
                              newBallDirection = getBallDirection resHit newBallPos ballDirection
                              newResult | bricksLeftUpdated == 0 = Win
                                        | checkFall newBallPos state = Lose
                                        | otherwise = NotFinished
                              newPlatformPos = checkAndMovePlatform state
                                     -- TODO Добавить выталкивание мяча


-- Draws picture in window for current game state
draw :: GameState -> Picture
draw GameState {..} | result == Win = Translate (- windowWidthFloat / 4) 0 $ Color black $ Text "Win!"
                    | result == Lose = Translate (- windowWidthFloat / 4) 0 $ Color black $ Text "Lose"
                    | otherwise = Pictures [ball, bricks, platform, walls]
                      where
                        ball = uncurry Translate ballPos (circleSolid ballRadius)
                        platform = uncurry Translate platformPos (rectangleSolid platformLength platformHeight)
                        bricks = drawGrid grid
                        walls = Pictures [
                          Translate 0 (windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate 0 (- windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate ((- windowWidthFloat) / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat),
                          Translate (windowWidthFloat / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat)]


-- Handles incoming events
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  | key == KeyLeft = state {keysPressed = if keyState == Down then LeftPressed:keysPressed else removeFromList keysPressed LeftPressed}
  | key == KeyRight = state {keysPressed = if keyState == Down then RightPressed:keysPressed else removeFromList keysPressed RightPressed}
  | otherwise = state
eventHandler _ state = state

run :: IO()
run = do
  gen <- getStdGen
  play window bgColor fps (initState (fst (randomR randRange gen))) draw eventHandler tick
