module Base where

import Graphics.Gloss.Interface.Pure.Game

import Lib

-- Draws picture in window for current game state
draw :: GameState -> Picture
draw (GameState isPlaying view ballPos ballDirection platformPos level grid bricksLeft result) = Pictures [ball]
  where
    ball = uncurry Translate ballPos (circleSolid 10)
draw _ = Blank

-- Handles incoming events
eventHandler :: Event -> GameState -> GameState
eventHandler _ state = state

-- Changes game state with each tick
tick :: Float -> GameState -> GameState
tick _ state = state