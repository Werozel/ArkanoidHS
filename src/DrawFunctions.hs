{-# LANGUAGE RecordWildCards #-}
module DrawFunctions where

import Graphics.Gloss.Interface.Pure.Game

import Lib


-- Draws full blocks grid
drawGrid :: BricksGrid -> Picture
drawGrid BricksGrid{..} = drawBricks bricks


drawBricks :: [[Brick]] -> Picture
drawBricks (row:xs) = Pictures [drawBricksRow row, drawBricks xs]
drawBricks _ = Pictures [Blank]


-- Draws one bricks row
drawBricksRow :: BricksGridRow -> Picture
drawBricksRow (NoBrick:xs) = Pictures [drawBricksRow xs]
drawBricksRow (brick@Brick {..}:xs) = Pictures [Color (getBrickColor brick) $ uncurry Translate position (uncurry rectangleSolid size), drawBricksRow xs]
drawBricksRow _ = Pictures [Blank]


-- Returns color of the brick
getBrickColor :: Brick -> Color
getBrickColor brick@Brick{..} = newColor
  where newColor | hitsLeft == 1 = yellow
                 | hitsLeft == 2 = orange
                 | hitsLeft == 3 = dark red