{-# LANGUAGE RecordWildCards #-}
module Run
  ( run, tick, initState
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
initState rnd = GameState True LevelView initBallPos initBallDirection initPlatformPos 0 initGrid 3 NotFinished [NonePressed]
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
tick :: Float -> GameState -> GameState
tick _ state@GameState{..} | result == Win =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                           | result == Lose =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Lose [NonePressed]
                           | otherwise = GameState isPlaying view newBallPos newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
                            where
                              newBallPos = moveBall ballPos ballDirection
                              newGrid = detectHit newBallPos (bricks grid)
                              hit = lastHit newGrid
                              PlatformHitResult platformHitFlag fromPlatformDirection = checkPlatformHit newBallPos state
                              resHit | platformHitFlag = PlatformHit
                                     | otherwise = hit
                              bricksLeftUpdated = getRemainingBricksCount newGrid
                              newBallDirection | platformHitFlag = fromPlatformDirection
                                               | otherwise = getBallDirection resHit newBallPos ballDirection
                              newResult | bricksLeftUpdated == 0 = Win
                                        | checkFall newBallPos state = Lose
                                        | otherwise = NotFinished
                              newPlatformPos = checkAndMovePlatform state
                                     -- TODO Добавить выталкивание мяча


-- Draws picture in window for current game state
draw :: GameState -> Picture
draw GameState {..} | result == Win = Pictures [winText, platform, walls]
                    | result == Lose = Pictures [loseText, ball, platform, walls]
                    | otherwise = Pictures [ball, bricks, platform, walls]
                      where
                        winText = Translate (- windowWidthFloat / 4) 0 $ Color black $ Text "Win!"
                        loseText = Translate (- windowWidthFloat / 3) 0 $ Color black $ Text "Lose"
                        ball = uncurry Translate ballPos (circleSolid ballRadius)
                        bricks = drawGrid grid
                        walls = Pictures [
                          Translate 0 (windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate 0 (- windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate ((- windowWidthFloat) / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat),
                          Translate (windowWidthFloat / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat)]
                        platformBorder = Color white $ rectangleSolid 1 platformHeight
                        platformBorders = Pictures [
                          Translate (fst platformPos + 1 + (platformLength / 2)) (snd platformPos) platformBorder,
                          Translate (fst platformPos - 1 - (platformLength / 2)) (snd platformPos) platformBorder,
                          uncurry Translate platformPos $ Color white $ circleSolid 1]
                        platform = Pictures [
                          uncurry Translate platformPos (rectangleSolid platformLength platformHeight),
                          platformBorders]


-- Handles incoming events
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  | key == KeyLeft = state {keysPressed = if keyState == Down then LeftPressed:keysPressed else removeFromList keysPressed LeftPressed}
  | key == KeyRight = state {keysPressed = if keyState == Down then RightPressed:keysPressed else removeFromList keysPressed RightPressed}
  | otherwise = state
eventHandler _ state = state


-- Runs the game
run :: IO()
run = do
  gen <- getStdGen
  play window bgColor fps (initState (fst (randomR randRange gen))) draw eventHandler tick
