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
    initBallPos = (50, initBallPositionY)

    initBallDirection :: Point
--    initBallDirection = (rnd, initBallDirectionY)
    initBallDirection = (initBallDirectionY, initBallDirectionY)

    initPlatformPos :: Point
    initPlatformPos = (0, initPlatformPositionY)
    
    initGrid = generateLevel 1


-- Changes game state with each tick
tick ::Float -> GameState -> GameState
tick _ state@GameState{..} | bricksLeft == 0 =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                     | otherwise = GameState isPlaying view ballPosRes newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
                      where
                        newBallPos = moveBall ballPos ballDirection
                        DetectHitResult newGrid ballPosRes = detectHit newBallPos (bricks grid)
                        hit = lastHit newGrid
                        bricksLeftUpdated = getRemainingBricksCount newGrid
                        newBallDirection = getBallDirection hit ballPosRes ballDirection
                        newResult | bricksLeftUpdated == 0 = Win
                               | otherwise = NotFinished
                        newPlatformPos = checkAndMovePlatform state
                               -- TODO Добавить Lose
                               -- TODO Добавить выталкивание мяча


-- Draws picture in window for current game state
draw :: GameState -> Picture
draw GameState {..} | result == Win = Pictures [winText, platform, walls]
                    | result == Lose = Pictures [loseText, platform, walls]
                    | otherwise = Pictures [ball, bricks, platform, walls]
                      where
                        ball = uncurry Translate ballPos (circleSolid ballRadius)
                        platform = uncurry Translate platformPos (rectangleSolid platformLength platformHeight)
                        bricks = drawGrid grid
                        winText = Translate (- windowWidthFloat / 4) 0 $ Color black $ Text "Win!"
                        loseText = Translate (- windowWidthFloat / 4) 0 $ Color black $ Text "Lose"
                        walls = Pictures [
                          Translate 0 (windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate 0 (- windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate ((- windowWidthFloat) / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat),
                          Translate (windowWidthFloat / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat)]


-- Handles incoming events
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  | key == KeyLeft = state {keysPressed = newKeysPressedLeft}
  | key == KeyRight = state {keysPressed = newKeysPressedRight}
  | otherwise = state
                              where
                                newKeysPressedLeft | keyState == Down = LeftPressed:keysPressed
                                                   | otherwise = removeFromList keysPressed LeftPressed
                                newKeysPressedRight | keyState == Down = RightPressed:keysPressed
                                                    | otherwise = removeFromList keysPressed RightPressed
eventHandler _ state = state

run :: IO()
run = do
  gen <- getStdGen
  -- randomX <- fst (randomR randRange gen)
  play window bgColor fps (initState 6) draw eventHandler tick
