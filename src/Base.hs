module Base where

import Graphics.Gloss.Interface.Pure.Game

import Lib

-- Draws picture in window for current game state
draw :: GameState -> Picture
draw _ = Blank

-- Handles incoming events
eventHandler :: Event -> GameState -> GameState
eventHandler _ state = state

-- Changes game state with each tick
tick :: Float -> GameState -> GameState
tick _ state = state