module LevelGenerator where

import Lib
import Graphics.Gloss.Data.Picture
import Constants

generateLevel :: Int -> BricksGrid
generateLevel difficult = BricksGrid
                  [[Brick (-150, 100) (brickLength, brickHeight) singleHit,
                   Brick (0, 100.0) (brickLength, brickHeight) 1,
                   Brick (150, 100.0) (brickLength, brickHeight) 1]] NoHit
