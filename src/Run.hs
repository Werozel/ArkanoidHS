{-# LANGUAGE RecordWildCards #-}
module Run
  ( run, tick, initState
  ) where

import Graphics.Gloss.Interface.Pure.Game

import System.Random
import Data.List

import Lib
import Constants
import Base
import LevelGenerator
import DrawFunctions
import Design
import Save


--Возвращает начальное состояние игры
initState :: Float -> View ->  GameState
initState rnd v   = GameState False v  initBallPos initBallDirection initPlatformPos 0 initGrid 3  NotFinished [NonePressed]
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


-- Изменяет состояние игры с каждым тиком
tick ::Float -> GameState -> GameState
tick _ state@GameState{..} | view /= LevelView = state
                           | result == Win =
                                  GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                           | result == Lose =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Lose [NonePressed]
                           | view /= LevelView = state
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
                              saveCheck = saveResult state


-- Обрабатывает входящие события
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  |key == KeySpace && view == Menu = state {view = LevelView}
  |key == KeySpace && view == Pause = state{view = LevelView}
  |key == KeyF3 = state{view = Menu}
  |key == KeyF2 = state{view = Pause}
  |key == KeyF1 && view == Pause = initState 25 LevelView
  |key == KeyF1 && view == LevelView = initState 25 LevelView
  |key == KeyEnter && view == StartScreen = state{view = Menu}   -- Handles continue while in start screen
  | view /= LevelView = state   -- Фильтрует все входные сигналы если не играет прямо сейчас
  -- Left arrow key moves platform to the left
  | key == KeyLeft = state {keysPressed = if keyState == Down then LeftPressed:keysPressed else delete LeftPressed keysPressed}
  -- Right arrow key moves platform to the right
  | key == KeyRight = state {keysPressed = if keyState == Down then RightPressed:keysPressed else delete RightPressed keysPressed }
  | otherwise = state
eventHandler (EventKey (Char c) Down _ _ ) state@GameState{..}
  | view /= LevelView = state
  -- | c == 'm' = state{view = Menu}
--   | c == 'f' = state{view = Pause}
  | otherwise = state
eventHandler _ state = state



-- Запустить игру
run :: IO()
run = do
  gen <- getStdGen
  play  window bgColor fps (initState (fst (randomR randRange gen ))  StartScreen ) draw eventHandler tick
