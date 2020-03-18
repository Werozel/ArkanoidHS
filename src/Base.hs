{-# LANGUAGE RecordWildCards #-}
module Base where

import Graphics.Gloss.Interface.Pure.Game

import Lib
import Constants


-- Returns new ball position on tick
moveBall :: Point -> Vector -> Point
moveBall (x, y) (vx, vy) = (newX, newY)
  where
    leftWindowBorder = - windowWidthFloat / 2
    rightWindowBorder = windowWidthFloat / 2
    topWindowBorder = windowHeightFloat / 2
    bottomWindowBorder = - windowHeightFloat / 2
    newX | vx < 0 = max leftWindowBorder (x + vx)
         | vx > 0 = min rightWindowBorder (x + vx)
         | otherwise = x + vx
    newY | vy < 0 = max bottomWindowBorder (y + vy)
         | vy > 0 = min topWindowBorder (y + vy)
         | otherwise = y + vy



-- Returns new ball direction dependent on hit
getBallDirection :: Hit -> Point -> Vector -> Vector
getBallDirection hit ballPos ballDirection
  | hit == LeftHit || hit == RightHit 
    || ballLeftBorder <= -windowHorizontalRadius || ballRightBorder >= windowHorizontalRadius 
      = (-(fst ballDirection), snd ballDirection)
  | hit == TopHit || hit == BottomHit || hit == PlatformHit ||
    ballTopBorder >= windowVerticalRadius || ballBottomBorder <= -windowVerticalRadius
      = (fst ballDirection, -(snd ballDirection))
  | hit == LeftTopHit || hit == RightTopHit ||
    hit == LeftBottomHit || hit == RightBottomHit
      = (- (fst ballDirection), - (snd ballDirection))
  | otherwise = ballDirection
  where
    ballLeftBorder = fst ballPos - ballRadius
    ballRightBorder = fst ballPos + ballRadius
    ballTopBorder = snd ballPos + ballRadius
    ballBottomBorder = snd ballPos - ballRadius
    windowHorizontalRadius = windowWidthFloat / 2
    windowVerticalRadius = windowHeightFloat / 2


-- Checks if ball hit a brick
checkHit :: Point -> Brick -> Hit
checkHit (x, y) Brick{..} | topCorner >= bottomBorder && bottomCorner < bottomBorder &&
                            leftCorner <= rightBorder && rightCorner > rightBorder = LeftTopHit
                          | topCorner >= bottomBorder && bottomCorner < bottomBorder &&
                            rightCorner >= leftBorder && leftCorner < leftBorder = RightTopHit
                          | bottomCorner >= topBorder && topCorner < topBorder &&
                            leftCorner <= rightBorder && rightCorner > rightBorder = LeftBottomHit
                          | bottomCorner >= topBorder && topCorner < topBorder &&
                            rightCorner >= leftBorder && leftCorner < leftBorder = RightBottomHit
                            
                          | leftBorder <= x && x <= rightBorder &&
                            ballTop > topBorder && ballBottom < topBorder = TopHit
                          | leftBorder <= x && x <= rightBorder &&
                            ballTop > bottomBorder && ballBottom < bottomBorder = BottomHit
                          | bottomBorder <= y && y <= topBorder &&
                            ballLeft < leftBorder && ballRight > leftBorder = LeftHit
                          | bottomBorder <= y && y <= topBorder &&
                            ballLeft < rightBorder && ballRight > rightBorder = RightHit
                          | otherwise = NoHit
        where
          leftBorder = fst position - (fst size / 2)
          rightBorder = fst position + (fst size / 2)
          topBorder = snd position + (snd size / 2)
          bottomBorder = snd position - (snd size / 2)
          
          sqrtTwo = sqrt 2
          topCorner = y + (ballRadius / sqrtTwo)
          bottomCorner = y - (ballRadius / sqrtTwo)
          leftCorner = x - (ballRadius / sqrtTwo)
          rightCorner = x + (ballRadius / sqrtTwo)
          
          ballTop = y + ballRadius
          ballBottom = y - ballRadius
          ballLeft = x - ballRadius
          ballRight = x + ballRadius
--          topCorner = ballTop
--          bottomCorner = ballBottom
--          leftCorner = ballLeft
--          rightCorner = ballRight


-- Result data from checkHit function
data CheckHitResult = CheckHitResult {
  row :: BricksGridRow, -- Altered row after hit
  hit :: Hit -- Hit that happened
}

-- Checks hit for each row
checkHitRow :: Point -> BricksGridRow -> CheckHitResult
checkHitRow _ [] = CheckHitResult [] NoHit
checkHitRow currPos (brick@Brick{..} : xs) 
                                 | resHit == NoHit = CheckHitResult (brick : resRow) resHitRow
                                 | otherwise = CheckHitResult (newBrick : xs) resHit
                          where
                            resHit = checkHit currPos brick
                            newBrick = if hitsLeft <= 1 then NoBrick else Brick position size (hitsLeft - 1)
                            CheckHitResult resRow resHitRow = checkHitRow currPos xs
checkHitRow currPos (brick@NoBrick : xs) = CheckHitResult (brick : resRow) resHitRow
                                          where
                                            CheckHitResult resRow resHitRow = checkHitRow currPos xs



-- Starts hit check
detectHit :: Point -> [BricksGridRow] -> BricksGrid
detectHit _ [] = BricksGrid [] NoHit
detectHit currPos (row: xs) | resHit == NoHit = BricksGrid (row : bricks) lastHit
                            | otherwise = BricksGrid (resRow : xs) resHit
                              where
                                CheckHitResult resRow resHit = checkHitRow currPos row
                                BricksGrid bricks lastHit = detectHit currPos xs


-- Result data from function checkPlatformHit
data PlatformHitResult = PlatformHitResult {
  hitFlag :: Bool,
  fromPlatformDirection :: Point -- New ball direction if ball hit the platform
}

-- Checks if ball hit the platform
checkPlatformHit :: Point -> GameState -> PlatformHitResult
checkPlatformHit (x, y) state@GameState{..} | platformX - platformLength / 2 <= x && x <= platformX + platformLength / 2 &&
                                              platformY - platformHeight / 2 <= ballBottom && ballBottom <= platformY + platformHeight / 2
                                               = PlatformHitResult True (ballNewXDirection / fromIntegral fps, ballNewYDirection / fromIntegral fps)
                                            | otherwise = PlatformHitResult False (0, 0)
                                                where
                                                  ballBottom = y - ballRadius
                                                  platformX = fst platformPos
                                                  platformY = snd platformPos
                                                  ballXFromPlatformPos = x - platformX
                                                  ballNewXDirection = ballXFromPlatformPos / (platformLength / 2) * snd platformHitAngleRange
                                                  ballNewYDirection = sqrt ((ballSpeed * ballSpeed) - (ballNewXDirection * ballNewXDirection))


-- Checks if ball has fallen beyond reach of the platform
checkFall :: Point -> GameState -> Bool
checkFall (x, y) state@GameState{..} | y - ballRadius < snd platformPos - (platformHeight / 2) - (ballRadius * 2.5) = True
                                     | otherwise = False


-- Returns new platform position
checkAndMovePlatform :: GameState -> Point
checkAndMovePlatform state = platformPos (checkAndMovePlatformRight (checkAndMovePlatformLeft state))


-- Moves platform left
checkAndMovePlatformLeft :: GameState -> GameState
checkAndMovePlatformLeft state@GameState{..}
  | LeftPressed `elem` keysPressed = state{platformPos = (max ((-windowWidthFloat + platformLength) / 2)
      (fst platformPos - (platformSpeed / fromIntegral Constants.fps)), initPlatformPositionY)}
  | otherwise = state

-- moves platform right
checkAndMovePlatformRight :: GameState -> GameState
checkAndMovePlatformRight state@GameState{..}
  | RightPressed `elem` keysPressed  = state{platformPos = (min ((windowWidthFloat - platformLength) / 2)
      (fst platformPos + (platformSpeed / fromIntegral Constants.fps)), initPlatformPositionY)}
  | otherwise = state


-- Returns number of bricks left on the row
getRemainingBricksCountRow :: BricksGridRow -> Int
getRemainingBricksCountRow [] = 0
getRemainingBricksCountRow (NoBrick : xs) = getRemainingBricksCountRow xs
getRemainingBricksCountRow _ = 1

-- Returns number of bricks left on the level
getRemainingBricksCount :: BricksGrid -> Int
getRemainingBricksCount (BricksGrid [] hit) = 0
getRemainingBricksCount (BricksGrid (row : xs) hit) = getRemainingBricksCountRow row + getRemainingBricksCount (BricksGrid xs hit)
