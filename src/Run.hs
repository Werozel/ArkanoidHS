{-# LANGUAGE RecordWildCards #-}
module Run
  ( run
  ) where

import Graphics.Gloss.Interface.Pure.Game

import System.Random

import Lib
import Constants
import Base
import LevelGenerator


-- Returns initial game state
initState :: Float -> GameState
initState rnd = GameState False MainMenu initBallPos initBallDirection initPlatformPos 0 initGrid 3 NotFinished [NonePressed]
  where
    initBallPos :: Point
    initBallPos = (0, initBallPositionY)

    initBallDirection :: Point
    initBallDirection = (rnd, initBallDirectionY)

    initPlatformPos :: Point
    initPlatformPos = (0, initPlatformPositionY)
    
    initGrid = generateLevel 1


-- Changes game state with each tick
tick ::Float -> GameState -> GameState
tick _ state@GameState{..} | bricksLeft == 0 =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                     | otherwise = GameState isPlaying view newBallPos newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
                      where
                        newBallPos = moveBall ballPos ballDirection
                        newGrid = detectHit newBallPos (bricks grid)
                        hit = lastHit newGrid
                        bricksLeftUpdated = getRemainingBricksCount newGrid
                        newBallDirection = getBallDirection hit newBallPos ballDirection
                        newResult | bricksLeftUpdated == 0 = Win
                               | otherwise = NotFinished
                        newPlatformPos = checkAndMovePlatform state
                               -- TODO Добавить Lose
                               -- TODO Добавить выталкивание мяча
                               
                               
-- Draws picture in window for current game state
draw :: GameState -> Picture
draw GameState {..} = Pictures [ball, bricks, platform, walls]
  where
    ball = uncurry Translate ballPos (circleSolid ballRadius)
    platform = uncurry Translate platformPos (rectangleSolid platformLength platformHeight)
    bricks = drawGrid grid
    walls = Pictures [
      Translate 0 (windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
      Translate 0 (- windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
      Translate ((- windowWidthFloat) / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat),
      Translate (windowWidthFloat / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat)]


run :: IO()
run = do
  gen <- getStdGen
  play window bgColor fps (initState (fst (randomR randRange gen))) draw eventHandler tick
