module Run
  ( run
  ) where

import Graphics.Gloss.Interface.Pure.Game

import System.Random

import Lib
import Constants
import Base
import LevelGenerator


-- Returns background color for window
bgColor :: Color
bgColor = greyN bgGreyN

-- Returns window for the game
window :: Display
window = InWindow "Aracnoid" (windowWidth, windowHeight) (windowOffsetX, windowOffsetY)



-- Returns initial game state
initState :: Float -> GameState
initState rnd = GameState False MainMenu initBallPos initBallDirection initPlatformPos 0 (BricksGrid [] NoHit) 1 NotFinished
  where
    initBallPos :: Point
    initBallPos = (0, initBallPositionY)

    initBallDirection :: Point
    initBallDirection = (rnd, initBallDirectionY)

    initPlatformPos :: Point
    initPlatformPos = (0, initPlatformPositionY)





run :: IO()
run = do
  gen <- getStdGen
  play window bgColor fps (initState (fst (randomR randRange gen))) draw eventHandler tick
