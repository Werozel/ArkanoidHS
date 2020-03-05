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
    walls = Pictures [Line (- windowWidthFloat / 2) (windowHeightFloat / 2)]


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


data CheckHitResult = CheckHitResult {
  row :: BricksGridRow,
  hit :: Hit
}


checkHit :: Point -> Brick -> Hit
checkHit (x, y) Brick{..} | leftBorder <= x && x <= rightBorder &&
                            ballTop > topBorder && ballBottom < topBorder = TopHit
                          | leftBorder <= x && x <= rightBorder &&
                            ballTop > bottomBorder && ballBottom < bottomBorder = BottomHit
                          | bottomBorder <= y && y <= topBorder &&
                            ballLeft < leftBorder && ballRight > leftBorder = LeftHit
                          | bottomBorder <= y && y <= topBorder &&
                            ballLeft < rightBorder && ballRight > rightBorder = RightHit
                          | otherwise = NoHit
        where
          leftBorder = fst position - fst size / 2
          rightBorder = fst position + fst size / 2
          topBorder = snd position + snd size / 2
          bottomBorder = snd position - snd size / 2
          ballTop = y + ballRadius
          ballBottom = y - ballRadius
          ballLeft = x - ballRadius
          ballRight = x + ballRadius


checkHitRow :: Point -> BricksGridRow -> CheckHitResult
checkHitRow _ [] = CheckHitResult [] NoHit
checkHitRow currPos (brick@Brick{..} : xs) 
                                 | resHit == NoHit = CheckHitResult (brick : resRow) resHitRow
                                 | otherwise = CheckHitResult (newBrick : xs) resHit
                          where
                            resHit = checkHit currPos brick
                            newBrick = if hitsLeft == 1 then NoBrick else Brick position size (hitsLeft - 1)
                            CheckHitResult resRow resHitRow = checkHitRow currPos xs



detectHit :: Point -> [BricksGridRow] -> BricksGrid
detectHit _ [] = BricksGrid [] NoHit
detectHit currPos (row: xs) | resHit == NoHit = BricksGrid (row : bricks) lastHit
                            | otherwise = BricksGrid (resRow : xs) resHit
                              where
                                CheckHitResult resRow resHit = checkHitRow currPos row
                                BricksGrid bricks lastHit = detectHit currPos xs


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


-- Changes game state with each tick
tick :: Float -> GameState -> GameState
tick _ state@GameState{..} | bricksLeft == 0 = GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                     | otherwise = GameState isPlaying view newBallPos newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
                      where
                        newBallPos = moveBall ballPos ballDirection
                        newGrid = detectHit newBallPos (bricks grid)
                        hit = lastHit newGrid
                        bricksLeftUpdated | hit == NoHit = bricksLeft
                                          | otherwise = bricksLeft - 1
                        newBallDirection = getBallDirection hit newBallPos ballDirection
                        newResult | bricksLeftUpdated == 0 = Win
                               | otherwise = NotFinished
                        newPlatformPos = checkAndMovePlatform state
                               -- TODO Добавить Lose
