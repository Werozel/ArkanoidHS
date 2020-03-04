{-# LANGUAGE RecordWildCards #-}
module Base where

import Graphics.Gloss.Interface.Pure.Game

import Lib
import Constants

-- Draws picture in window for current game state
draw :: GameState -> Picture
draw GameState{..} = Pictures [ball]
  where
    ball = uncurry Translate ballPos (circleSolid ballRadius)


-- Handles incoming events
eventHandler :: Event -> GameState -> GameState
eventHandler _ state = state


moveBall :: Point -> Vector -> Point
moveBall (x, y) (vx, vy) = (x + vx, y + vy)

getBallDirection :: Hit -> Point -> Vector -> Vector
getBallDirection hit ballPos ballDirection
  | hit == LeftHit || hit == RightHit 
    || ballLeftBorder <= -windowHorizontalRadius || ballRightBorder >= windowHorizontalRadius 
      = (-(fst ballDirection), snd ballDirection)
  | hit == TopHit || hit == BottomHit || hit == PlatformHit
    || ballTopBorder >= windowVerticalRadius || ballBottomBorder <= -windowVerticalRadius
      = (fst ballDirection, -(snd ballDirection))
  | otherwise = ballDirection
  where
    ballLeftBorder = fst ballPos - ballRadius
    ballRightBorder = fst ballPos + ballRadius
    ballTopBorder = snd ballPos + ballRadius
    ballBottomBorder = snd ballPos - ballRadius
    windowHorizontalRadius = windowWidthFloat / 2
    windowVerticalRadius = windowHeightFloat / 2

detectHit :: Point -> BricksGrid -> BricksGrid
detectHit _ g = g


-- Changes game state with each tick
tick :: Float -> GameState -> GameState
tick _ GameState{..} | bricksLeft == 0 = GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win
                     | otherwise = GameState isPlaying view newBallPos newBallDirection platformPos level newGrid bricksLeftUpdated newResult
                      where
                        newBallPos = moveBall ballPos ballDirection
                        newGrid = detectHit newBallPos grid
                        hit = lastHit newGrid
                        bricksLeftUpdated | hit == NoHit = bricksLeft
                                          | otherwise = bricksLeft - 1
                        newBallDirection = getBallDirection hit newBallPos ballDirection
                        newResult | bricksLeftUpdated == 0 = Win
                               | otherwise = NotFinished
                               -- TODO Добавить Lose
