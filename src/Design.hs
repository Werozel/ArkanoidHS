{-# LANGUAGE RecordWildCards #-}
module Design where
import Graphics.Gloss.Interface.Pure.Game

import System.Random
import Data.List

import Lib
import Constants
import Base
import LevelGenerator
import DrawFunctions


-- Рисует картинку в окне для текущего состояния игры
draw :: GameState -> Picture
draw GameState {..} | view == StartScreen = Scale 0.33 0.35 $ Pictures [startScreen]
                    | view == Menu = Scale 0.45 0.45 $ Pictures [menu]
                    | result == Win = Pictures [namesGame, winGame, help, gameboy, platform, wallsCollor, bricks, ball]
                    | result == Lose = Pictures [namesGame, loseGame, help, gameboy, platform, wallsCollor, bricks, ball]
                    | view == Pause = Pictures [namesGame, help, gameboy, platform, wallsCollor, bricks, ball, paused]
                    | otherwise = Pictures [namesGame, help, gameboy, platform, wallsCollor, bricks, ball]
                      where
                        --Pause
                        paused = pause
                        pause  = Pictures [
                          Scale 0.35 0.35 $ Translate (-windowWidthFloat * 0.67) 0 $ Color yellow $ Text "PAUSED",
                          Scale 0.35 0.35 $  Translate (-windowWidthFloat * 0.66) 0 $ Color yellow $ Text "PAUSED"]

                      --LevelView
                        namesGame = name
                        name = Pictures [
                          Scale 0.72 0.72 $ Rotate (-90) $ Translate (-380) (-420) $ Color cyan $ Text "GAMEBOY",
                          Scale 0.72 0.72 $ Rotate (-90) $ Translate (-384) (-424) $ Color azure $ Text "GAMEBOY",
                          Scale 0.72 0.72 $ Rotate (-90)  $ Translate (-387) (-424) $ Color magenta $ Text "GAMEBOY",
                          Translate (-windowWidthFloat * 2.0 ) 330 $ Color azure $ Text "ARKANOID",
                          Translate (-windowWidthFloat * 1.98 ) 334 $ Color magenta $ Text "ARKANOID",
                          Translate (-windowWidthFloat * 1.99 ) 334 $ Color cyan $ Text "ARKANOID"]

                        winGame = wins
                        wins = Pictures [
                            Translate (- windowWidthFloat / 4) 0 $ Color yellow $ Text "Win!",
                            Translate (- windowWidthFloat / 4) 2.6 $ Color yellow $ Text "Win!",
                            Translate (- windowWidthFloat / 4) 1.6 $ Color yellow $ Text "Win!"]

                        loseGame = los
                        los = Pictures [
                            Translate (- windowWidthFloat / 3) 0 $ Color yellow $ Text "Lose",
                            Translate (- windowWidthFloat / 2.99) 2.6 $ Color yellow $ Text "Lose",
                            Translate (- windowWidthFloat / 2.99) 1.6 $ Color yellow $ Text "Lose"]

                        help = helps
                        helps = Pictures [
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.8) 150 $ Color yellow $ Text "| Press 'F3' - to enter MENU ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.809) 150 $ Color yellow $ Text "| Press 'F3' - to enter MENU ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.8) 0 $ Color yellow $ Text "| Press 'F2' - to Paused     ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.809) 0 $ Color yellow $ Text "| Press 'F2' - to Paused  ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.8) (-150) $ Color yellow $ Text "| Press 'F1' - to Restart     ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.809) (-150) $ Color yellow $ Text "| Press 'F1' - to Restart  ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.8) (-300) $ Color yellow $ Text "| Press 'Esc' - to exit Game  ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.809) (-300) $ Color yellow $ Text "| Press 'Esc' - to exit Game  ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.8) (-450) $ Color yellow $ Text "| Press 'Space' - to play Game  ",
                          Scale 0.22 0.22 $ Translate (-windowWidthFloat * 8.809) (-450) $ Color yellow $ Text "| Press 'Space' - to play Game  "]

                        gameboy = Color azure gameboys
                        gameboys = Pictures [
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
                          Rotate (-30) $ Translate (17) 401 (rectangleSolid windowWidthScore wallsWidth),
                          Rotate (-30) $ Translate (404) 174 (rectangleSolid windowWidthScore wallsWidth),
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


                        --Menu
                        menu = menuSetting
                        menuSetting = Pictures [
                            Scale 2.1 2.1 $ Translate (-windowWidthFloat * 0.8 ) 280 $ Color azure $ Text "ARKANOID",
                            Scale 2.1 2.1 $ Translate (-windowWidthFloat * 0.78 ) 284 $ Color magenta $ Text "ARKANOID",
                            Scale 2.1 2.1 $ Translate (-windowWidthFloat * 0.79 ) 284 $ Color cyan $ Text "ARKANOID",

                            Translate (-windowWidthFloat * 1.7) 370 $ Color yellow $ Text "Menu",
                            Translate (-windowWidthFloat * 1.69) 370 $ Color yellow $ Text "Menu",
                            Translate (-windowWidthFloat * 1.7) (-370) $ Color yellow $ Text "Bonus",
                            Translate (-windowWidthFloat * 1.69) (-370) $ Color yellow $ Text "Bonus",

                            Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 300 $ Color white $ Text "- control arrow  <- | ->",
                            Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 150 $ Color white $ Text "- 'F1' restart game",
                            Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 0 $ Color white $ Text "- 'F2' paused game",
                            Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) (-150) $ Color white $ Text "- 'Esc' to exit game ",
                            Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) (-300) $ Color white $ Text "- Press 'Space' to play",
                            Scale 0.65 0.70 $  Translate (-windowWidthFloat * 1.6) (-750) $ Color white $ Text "Hitting a corner of a brick - one shot it"]

                        --StartScreen
                        startScreen = startScreenSetting
                        startScreenSetting = Pictures [
                              Translate (-windowWidthFloat * 2.8) 350 $ Color yellow $ Text "Hello",
                              Translate (-windowWidthFloat * 2.5) 110 $ Color white $ Text "Welcome to the game ARKANOID!",
                              Translate (-windowWidthFloat * 1.5) (-100) $ Color white $ Text "Press 'Enter' to play"]


                        ball = uncurry Translate ballPos $ Color white (circleSolid ballRadius)

                        bricks = drawGrid grid
                        hitText | lastHit grid == NoHit = Blank
                                | otherwise = Translate (-windowWidthFloat / 2) 0 $ Color black $ Text (showHit (lastHit grid))

                        wallsCollor = Color cyan walls
                        walls= Pictures [
                          Translate 0 (windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate 0 (- windowHeightFloat / 2.0) (rectangleSolid windowWidthFloat wallsWidth),
                          Translate ((- windowWidthFloat) / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat),
                          Translate (windowWidthFloat / 2.0) 0 (rectangleSolid wallsWidth windowHeightFloat)
                          ]

                        platformBorder = Color white $ rectangleSolid 2 platformHeight
                        platformBorders = Pictures [
                          Translate (fst platformPos + 1 + (platformLength / 2)) (snd platformPos) platformBorder,
                          Translate (fst platformPos - 1 - (platformLength / 2)) (snd platformPos) platformBorder,
                          uncurry Translate platformPos $ Color white $ circleSolid 3]
                        platform = Pictures [
                          uncurry Translate platformPos $ Color violet (rectangleSolid platformLength platformHeight),
                          platformBorders]
