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
initState rnd = GameState False MainMenu initBallPos initBallDirection initPlatformPos 0 initGrid 0 NotFinished
  where
    initBallPos :: Point
    initBallPos = (0, initBallPositionY)

    initBallDirection :: Point
    initBallDirection = (rnd, initBallDirectionY)

    initPlatformPos :: Point
    initPlatformPos = (0, initPlatformPositionY)
    
    initGrid = generateLevel 1


run :: IO()
run = do
  gen <- getStdGen
  play window bgColor fps (initState (fst (randomR randRange gen))) draw eventHandler tick
