module Lib where

import Graphics.Gloss.Interface.Pure.Game

import Data.Time.Clock

import System.Random

import Constants

-- Brick data structure
data Brick = Brick {
  position :: Point,
  size :: Point,
  hitsLeft :: Int
} | NoBrick


-- Types of ball hit
data Hit = LeftHit | TopHit | RightHit | BottomHit | NoHit | PlatformHit deriving Eq
-- A single row from a grid
type BricksGridRow = [Brick]
-- All bricks
data BricksGrid = BricksGrid {
  bricks :: [BricksGridRow],
  lastHit :: Hit
}

-- Result of a game
data Result = Win | Lose | NoTime | NotFinished
-- Current menu or level
data View = MainMenu | ResultsMenu | SettingsMenu | LevelView

data KeyPressed = LeftPressed | RightPressed | NonePressed deriving Eq
type KeysPressed = [KeyPressed]

-- Game state data structure
-- Point = (Float, Float)
data GameState = GameState {
  isPlaying :: Bool, -- Flag indicates weather a game is in progress of waiting to be started
  view :: View, -- Specifies what state has the window
  ballPos :: Point, -- Current position of a ball
  ballDirection :: Vector, -- Vector = Point; Current direction of a ball
  platformPos :: Point, -- Current position of a platform
  level :: Int, -- Current level (if playing, else = 0)
  grid :: BricksGrid, -- Bricks layout
  bricksLeft :: Int, -- Bricks left to be removed
  result :: Result, -- result of a game or NotFinished flag
  keysPressed :: KeysPressed -- Key that is pressed at the moment
}

-- Returns background color for window
bgColor :: Color
bgColor = greyN bgGreyN


-- Returns window for the game
window :: Display
window = InWindow "Aracnoid" (windowWidth, windowHeight) (windowOffsetX, windowOffsetY)

