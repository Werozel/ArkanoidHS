{-# LANGUAGE RecordWildCards #-}
module Base where

import Graphics.Gloss.Interface.Pure.Game

import Lib
import Constants
import Service



moveBall :: Point -> Vector -> Point
moveBall (x, y) (vx, vy) = (newX, newY)
  where
    leftWindowBorder = - windowWidthFloat / 2
    rightWindowBorder = windowWidthFloat / 2
    topWindowBorder = windowHeightFloat / 2
    bottomWindowBorder = - windowHeightFloat / 2
    -- FIXME Координата неправильная
    newX | vx < 0 = max leftWindowBorder (x + vx + ballRadius)
         | vx > 0 = min rightWindowBorder (x + vx - ballRadius)
         | otherwise = x + vx
    newY | vy < 0 = max bottomWindowBorder (y + vy + ballRadius)
         | vy > 0 = min topWindowBorder (y + vy - ballRadius)
         | otherwise = y + vy



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
  hit :: Hit,
  newBallPos :: Point
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
checkHitRow pos [] = CheckHitResult [] NoHit pos
checkHitRow currPos (brick@Brick {..}:xs)
  | resHit == NoHit = CheckHitResult (brick : resRow) resHitRow newBallPos
  | otherwise = CheckHitResult (newBrick : xs) resHit newBallPos
  where
    resHit = checkHit currPos brick
    newBrick
      | hitsLeft <= 1 = NoBrick
      | otherwise = Brick position size (hitsLeft - 1)
    newBallPos
      | True = (0, 0)
      | resHit == TopHit = (0, 0)
      | resHit == BottomHit = (0, 0)
      | resHit == RightHit = (0, 0)
      | resHit == LeftHit = (0, 0)
      | otherwise = currPos
    verticalTopGap = abs (snd position - brickHeight / 2 - snd currPos - ballRadius)
    verticalBottomGap = abs (snd position + brickHeight / 2 - snd currPos + ballRadius)
    horizontalLeftGap = abs (fst position + brickLength / 2 - fst currPos + ballRadius)
    horizontalRightGap = abs (fst position - brickLength / 2 - fst currPos - ballRadius)
    CheckHitResult resRow resHitRow _ = checkHitRow currPos xs
checkHitRow currPos (brick@NoBrick : xs) = CheckHitResult (brick : resRow) resHitRow newBallPos
                                          where
                                            CheckHitResult resRow resHitRow newBallPos = checkHitRow currPos xs



data DetectHitResult = DetectHitResult {
  gridRes :: BricksGrid,
  newPos :: Point
}

detectHit :: Point -> [BricksGridRow] -> DetectHitResult
detectHit currPos [] = DetectHitResult (BricksGrid [] NoHit) currPos
detectHit currPos (row: xs) | resHit == NoHit = DetectHitResult (BricksGrid (row : bricks) lastHit) newBallPosNext
                            | otherwise = DetectHitResult (BricksGrid (resRow : xs) resHit) newBallPos
                              where
                                CheckHitResult resRow resHit newBallPos = checkHitRow currPos row
                                DetectHitResult (BricksGrid bricks lastHit) newBallPosNext = detectHit currPos xs

blankDetectHit :: Point -> [BricksGridRow] -> BricksGrid
blankDetectHit pos rows = BricksGrid rows NoHit



-----------------------------
-- Remaining bricks count  --
-----------------------------

getRemainingBricksCountRow :: BricksGridRow -> Int
getRemainingBricksCountRow [] = 0
getRemainingBricksCountRow (NoBrick : xs) = getRemainingBricksCountRow xs
getRemainingBricksCountRow _ = 1

getRemainingBricksCount :: BricksGrid -> Int
getRemainingBricksCount (BricksGrid [] hit) = 0
getRemainingBricksCount (BricksGrid (row : xs) hit) = getRemainingBricksCountRow row + getRemainingBricksCount (BricksGrid xs hit)



------------------------------
--    Platform Movement     --
------------------------------

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
