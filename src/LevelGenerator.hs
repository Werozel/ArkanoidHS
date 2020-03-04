module LevelGenerator where

import Lib
import Graphics.Gloss.Data.Picture
import Constants

generateLevel :: Int -> BricksGrid
generateLevel difficult = BricksGrid
                  [[Brick (brickHeight, brickLength) (brickHeight, brickLength) singleHit,
                   Brick (70.0, 100.0) (10.0, 10.0) 1,
                   Brick (40.0, 100.0) (10.0, 10.0) 1]] NoHit
