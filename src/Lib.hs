module Lib where

import Graphics.Gloss.Interface.Pure.Game

import Constants

-- TODO Сделать нормально
data GameState = GameState {
  state :: Int,
  evenMoreState :: Int
}

-- Returns background color for window
bgColor :: Color
bgColor = greyN bgGreyN

-- Returns window for the game
window :: Display
window = InWindow "Aracnoid" (windowWidth, windowHeight) (windowOffsetX, windowOffsetY)

-- Returns initial game state
initState :: GameState
initState = GameState 0 0


