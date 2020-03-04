module Lib where

import Graphics.Gloss.Interface.Pure.Game

import Data.Time.Clock

import System.Random

import Constants

-- Brick data structure
data Brick = Brick {
  position :: Point,
  hitsLeft :: Int
}


-- A single row from a grid
type BricksGridRow = [Brick]
-- All bricks
type BricksGrid = [BricksGridRow]
-- Result of a game
data Result = Win | Lose | NoTime | NotFinished
-- Current menu or level
data View = MainMenu | ResultsMenu | SettingsMenu | Level

-- TODO Сделать нормально
-- Game state data structure
data GameState = GameState {
  isPlaying :: Bool, -- Flag indicates weather a game is in progress of waiting to be started
  view :: View, -- Specifies what state has the window
  ballPos :: Point, -- Current position of a ball
  ballDirection :: Vector, -- Vector = Point; Current direction of a ball
  platformPos :: Point, -- Current position of a platform
  level :: Int, -- Current level (if playing, else = 0)
  grid :: BricksGrid, -- Bricks layout
  bricksLeft :: Int, -- Bricks left to be removed
  result :: Result -- result of a game or NotFinished flag
}

-- Returns background color for window
bgColor :: Color
bgColor = greyN bgGreyN

-- Returns window for the game
window :: Display
window = InWindow "Aracnoid" (windowWidth, windowHeight) (windowOffsetX, windowOffsetY)

-- Returns initial game state
initState :: Float -> GameState
initState rnd = GameState False MainMenu initBallPos initBallDirection initPlatformPos 0 [] 0 NotFinished
  where
    initBallPos :: Point
    initBallPos = (0, -150)

    initBallDirection :: Point
    initBallDirection = (rnd, initBallDirectionY)

    initPlatformPos :: Point
    initPlatformPos = (0, -50)



