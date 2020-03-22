{-# LANGUAGE RecordWildCards #-}
module Run
  ( run, tick, initState
  ) where

import Graphics.Gloss.Interface.Pure.Game

import System.Random
import Data.List

import Lib
import Constants
import Base
import LevelGenerator
import DrawFunctions


--Возвращает начальное состояние игры
initState :: Float -> View ->  GameState
initState rnd v  = GameState False v  initBallPos initBallDirection initPlatformPos 0 initGrid 3 NotFinished [NonePressed]
  where
    initBallPos :: Point
    initBallPos = (0, initBallPositionY)

    initBallDirection :: Point
    initBallDirection = (rnd / fromIntegral fps, ballVerticalDirection / fromIntegral fps)
      where
        ballVerticalDirection = sqrt ((ballSpeed * ballSpeed) - (rnd * rnd))

    initPlatformPos :: Point
    initPlatformPos = (0, initPlatformPositionY)

    initGrid = generateLevel 1


-- Изменяет состояние игры с каждым тиком
tick ::Float -> GameState -> GameState
tick _ state@GameState{..} | view /= LevelView = state
                           | result == Win =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                           | result == Lose =
                              GameState False LevelView ballPos (0, 0) platformPos level grid 0 Lose [NonePressed]
                           |view /= LevelView = state
                           | otherwise = GameState isPlaying view newBallPos newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
                            where
                              newBallPos = moveBall ballPos ballDirection
                              newGrid = detectHit newBallPos (bricks grid)
                              hit = lastHit newGrid
                              PlatformHitResult platformHitFlag fromPlatformDirection = checkPlatformHit newBallPos state
                              resHit | platformHitFlag = PlatformHit
                                     | otherwise = hit
                              bricksLeftUpdated = getRemainingBricksCount newGrid
                              newBallDirection | platformHitFlag = fromPlatformDirection
                                               | otherwise = getBallDirection resHit newBallPos ballDirection
                              newResult | bricksLeftUpdated == 0 = Win
                                        | checkFall newBallPos state = Lose
                                        | otherwise = NotFinished
                              newPlatformPos = checkAndMovePlatform state


-- Рисует картинку в окне для текущего состояния игры
draw :: GameState -> Picture
draw GameState {..} | view == StartScreen = Scale 0.25 0.25 $ Pictures [tutorialTextW,helloStr, tutorialTextContinue]
                    | view == Menu = Scale 0.35 0.35 $ Pictures [menuTextRestrat,menuTextControl1,menuTwxtContinue ]
                    | result == Win = Pictures [winText, winText2, winText3, nameGame,nameGame2, nameGame3, platform, wallsCollor, menu2, menu]
                    | result == Lose = Pictures [loseText, loseText2, loseText3, nameGame, nameGame2, nameGame3, ball, platform, wallsCollor, menu2, menu]
                    | view == Pause = Scale 0.50 0.50 $ Pictures [paused, paused2]
                    | otherwise = Pictures [ball, nameGame, nameGame2, nameGame3, bricks, platform, wallsCollor, menu,menu2]
                      where
                        paused = Translate (-windowWidthFloat * 0.67) 50 $ Color yellow $ Text "PAUSED"
                        paused2 = Translate (-windowWidthFloat * 0.66) 50 $ Color yellow $ Text "PAUSED"

                        helloStr = Translate (-windowWidthFloat * 2.8) 350 $ Color yellow $ Text "Hello, Artem "

                        nameGame = Translate (-windowWidthFloat * 2.1 ) 320 $ Color azure $ Text "ARKANOID"
                        nameGame2 = Translate (-windowWidthFloat * 2.08 ) 324 $ Color magenta $ Text "ARKANOID"
                        nameGame3 = Translate (-windowWidthFloat * 2.09 ) 323 $ Color cyan $ Text "ARKANOID"
                        menu = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.1) 150 $ Color magenta $ Text " Press m - to enter MENU"
                        menu2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.121) 151 $ Color magenta $ Text " Press m - to enter MENU"


                        menuTextRestrat = Translate (-windowWidthFloat * 1.5) 150 $ Color white $ Text "- Welcome"
                        menuTextControl1 = Translate (-windowWidthFloat * 1.5) 0 $ Color white $ Text "to the game"
                        menuTwxtContinue = Translate (-windowWidthFloat * 1.5) (-150) $ Color white $ Text "Press 'Space' to play"

                        tutorialTextW = Translate (-windowWidthFloat * 2.5) 110 $ Color white $ Text "Welcome to the game ARKANOID!"
                        tutorialTextContinue = Translate (-windowWidthFloat * 1.5) (-100) $ Color white $ Text "Press 'Space' to play"

                        winText = Translate (- windowWidthFloat / 4) 0 $ Color yellow $ Text "Win!"
                        winText2 = Translate (- windowWidthFloat / 4) 2.6 $ Color yellow $ Text "Win!"
                        winText3= Translate (- windowWidthFloat / 4) 1.6 $ Color yellow $ Text "Win!"

                        loseText = Translate (- windowWidthFloat / 3) 0 $ Color yellow $ Text "Lose"
                        loseText2 = Translate (- windowWidthFloat / 2.99) 2.6 $ Color yellow $ Text "Lose"
                        loseText3 = Translate (- windowWidthFloat / 2.99) 1.6 $ Color yellow $ Text "Lose"

                        ball = uncurry Translate ballPos $ Color white (circleSolid ballRadius)
                        bricks = drawGrid grid
                        hitText | lastHit grid == NoHit = Blank
                                | otherwise = Translate (-windowWidthFloat / 2) 0 $ Color black $ Text (showHit (lastHit grid))

                        wallsCollor = Color cyan (walls)
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


-- Обрабатывает входящие события
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  |key == KeySpace && view == Menu = state {view = LevelView}
  |key == KeySpace && view == Pause = state{view = LevelView}
  |key == KeySpace && view == StartScreen = state{view = LevelView}   -- Handles continue while in start screen
  | view /= LevelView = state   -- Фильтрует все входные сигналы если не играет прямо сейчас
  -- Left arrow key moves platform to the left
  | key == KeyLeft = state {keysPressed = if keyState == Down then LeftPressed:keysPressed else delete LeftPressed keysPressed}
  -- Right arrow key moves platform to the right
  | key == KeyRight = state {keysPressed = if keyState == Down then RightPressed:keysPressed else delete RightPressed keysPressed }
  | otherwise = state
eventHandler (EventKey (Char c) Down _ _ ) state@GameState{..}
  | view /= LevelView = state
  | c == 'm' = state{view = Menu}
  | c == 'f' = state{view = Pause}
  | c == 'r' || c == 'к' = initState 25 LevelView    -- Handles restart button
  | otherwise = state
eventHandler _ state = state


-- Запустить игру
run :: IO()
run = do
  putStrLn "Hello, what's your name?"
  name <-getLine
  gen <- getStdGen
  play  window bgColor fps (initState (fst (randomR randRange gen ))  StartScreen) draw eventHandler tick
