{-# LANGUAGE RecordWildCards #-}
module Base where

import Graphics.Gloss.Interface.Pure.Game

import Lib
import Constants
import Service

-- Draws picture in window for current game state
draw :: GameState -> Picture
draw GameState{..} = Pictures [ball, bricks, platform]
  where
    ball = uncurry Translate ballPos (circleSolid ballRadius)
    platform = uncurry Translate platformPos (rectangleSolid platformLength platformHeight)
    bricks = drawGrid grid


-- Draws full blocks grid
drawGrid :: BricksGrid -> Picture
drawGrid BricksGrid{..} = drawBricks bricks

drawBricks :: [[Brick]] -> Picture
drawBricks (row:xs) = Pictures [drawBricksRow row, drawBricks xs]
drawBricks _ = Pictures [Blank]


-- Draws one bricks row
drawBricksRow :: BricksGridRow -> Picture
drawBricksRow (NoBrick:xs) = Pictures [drawBricksRow xs]
drawBricksRow (Brick {..}:xs) = Pictures [uncurry Translate position (uncurry rectangleSolid size), drawBricksRow xs]
drawBricksRow _ = Pictures [Blank]


-- Handles incoming events
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  | key == KeyLeft = state {keysPressed = if keyState == Down then LeftPressed:keysPressed else removeFromList keysPressed LeftPressed}
  | key == KeyRight = state {keysPressed = if keyState == Down then RightPressed:keysPressed else removeFromList keysPressed RightPressed}
  | otherwise = state
eventHandler _ state = state




moveBall :: Point -> Vector -> Point
moveBall (x, y) (vx, vy) = (x + vx, y + vy)

-- FIXME Бесконечно ударяется о верхнюю границу
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


checkAndMovePlatform :: GameState -> Point
checkAndMovePlatform state = platformPos (checkAndMovePlatformRight (checkAndMovePlatformLeft state))


checkAndMovePlatformLeft :: GameState -> GameState
checkAndMovePlatformLeft state@GameState{..}
  | elemInList keysPressed LeftPressed = state{platformPos = (max ((-windowWidthFloat + platformLength) / 2)
      (fst platformPos - (platformSpeed / fromIntegral Constants.fps)), initPlatformPositionY)}
  | otherwise = state

checkAndMovePlatformRight :: GameState -> GameState
checkAndMovePlatformRight state@GameState{..}
  | elemInList keysPressed RightPressed = state{platformPos = (min ((windowWidthFloat - platformLength) / 2)
      (fst platformPos + (platformSpeed / fromIntegral Constants.fps)), initPlatformPositionY)}
  | otherwise = state

detectHit :: Point -> BricksGrid -> BricksGrid
detectHit _ g = g


-- Changes game state with each tick
tick :: Float -> GameState -> GameState
tick _ state@GameState{..} | bricksLeft == 0 = GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                     | otherwise = GameState isPlaying view newBallPos newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
                      where
                        newBallPos = moveBall ballPos ballDirection
                        newGrid = detectHit newBallPos grid
                        hit = lastHit newGrid
                        bricksLeftUpdated | hit == NoHit = bricksLeft
                                          | otherwise = bricksLeft - 1
                        newBallDirection = getBallDirection hit newBallPos ballDirection
                        newResult | bricksLeftUpdated == 0 = Win
                               | otherwise = NotFinished
                        newPlatformPos = checkAndMovePlatform state
                               -- TODO Добавить Lose
