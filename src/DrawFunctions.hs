{-# LANGUAGE RecordWildCards #-}
module DrawFunctions where

import Graphics.Gloss.Interface.Pure.Game

import Lib


-- Рисует полную сетку блоков
drawGrid :: BricksGrid -> Picture
drawGrid BricksGrid{..} = drawBricks bricks


drawBricks :: [[Brick]] -> Picture
drawBricks (row:xs) = Pictures [drawBricksRow row, drawBricks xs]
drawBricks _ = Pictures [Blank]


-- Рисует один ряд кирпичей
drawBricksRow :: BricksGridRow -> Picture
drawBricksRow (NoBrick:xs) = Pictures [drawBricksRow xs]
drawBricksRow (brick@Brick {..}:xs) = Pictures [Color (getBrickColor brick) $ uncurry Translate position (uncurry rectangleSolid size), drawBricksRow xs]
drawBricksRow _ = Pictures [Blank]


--Возвращает цвет кирпича
getBrickColor :: Brick -> Color
getBrickColor brick@Brick{..} = newColor
  where newColor | hitsLeft == 3 = yellow
                 | hitsLeft == 2 = orange
                 | hitsLeft == 1 = dark red
