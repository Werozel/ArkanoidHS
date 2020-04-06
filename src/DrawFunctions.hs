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

getGameBoys :: Picture
getGameBoys = Pictures [
                            Translate 110 (-400)  $ Color (dark chartreuse) (circleSolid 22),
                            Translate 106 (-400) $ Color chartreuse (circleSolid 20),


                            Translate 110 (-340) $ Color (dark yellow)(circleSolid 22),
                            Translate 106 (-340) $ Color yellow (circleSolid 20),

                           Translate 66 (-370) $ Color (dark azure) (circleSolid 22),
                           Translate 62 (-370) $ Color azure (circleSolid 20),

                            Translate 156 (-370) $ Color (dark red) (circleSolid 22),
                            Translate 152 (-370) $ Color red (circleSolid 20),

                            Translate (-127) (-368) $ Color azure (rectangleSolid 40 100),
                            Translate (-127) (-368) $ Color azure (rectangleSolid 100 40),
                            Translate (-130) (-370) $ Color (dark cyan) (rectangleSolid 35 90),
                            Translate (-130) (-370) $ Color (dark cyan) (rectangleSolid 90 35),

                            Rotate (-30) $ Translate 17 401 (rectangleSolid windowWidthScore wallsWidth),
                            Rotate (-30) $ Translate 404 174 (rectangleSolid windowWidthScore wallsWidth),
                            Rotate (-30) $ Translate 21 (-500) (rectangleSolid windowWidthScore wallsWidth),
                            Translate 18 328 (rectangleSolid windowWidthFloat wallsWidth), -- вверх 1 слой
                            Translate (-30) 328 (rectangleSolid windowWidthFloat wallsWidth), -- вверх 1 слой
                            Translate 225 29 (rectangleSolid wallsWidth windowHeightFloat),  -- бок 1 слой
                            Translate 225 (-150) (rectangleSolid wallsWidth windowHeightFloat), -- бок слой 1 слой
                            Translate 27 (-448) (rectangleSolid windowWidthFloat wallsWidth), -- низ 1 слой
                            Translate (-28) (-448) (rectangleSolid windowWidthFloat wallsWidth), -- низ 1 слой
                            Translate (-225) 29 (rectangleSolid wallsWidth windowHeightFloat),  -- бок 1 слой
                            Translate (-225) (-150) (rectangleSolid wallsWidth windowHeightFloat),
                            Translate 107 378 (rectangleSolid windowWidthFloat wallsWidth), -- вверх 2 слой
                            Translate 50 378 (rectangleSolid windowWidthFloat wallsWidth),
                            Translate 309 80 (rectangleSolid wallsWidth windowHeightFloat),  -- бок 2 слой
                            Translate 309 (-97) (rectangleSolid wallsWidth windowHeightFloat)]
