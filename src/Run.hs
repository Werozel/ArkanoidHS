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
initState rnd v   = GameState False v  initBallPos initBallDirection initPlatformPos 0 initGrid 3  NotFinished [NonePressed]
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
draw GameState {..} | view == StartScreen = Scale 0.33 0.35 $ Pictures [helloStr,tutorialTextW, tutorialTextContinue]
                    | view == Menu = Scale 0.45 0.45 $ Pictures [menuText,menuText2, menuTextControl, menuTextRestrat, menuTextPaused,menuTextContine,menuTextEsc,menuTextBonus,menuTextBonus2,nameGame4,nameGame5,nameGame6, menuTextBon]
                    | result == Win = Pictures [winText, winText2,gameboy, winText3, nameGame,nameGame2, nameGame3, platform, wallsCollor, menu2, menu, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart ,menuRestart2,nameBoy,nameBoy2,nameBoy3]
                    | result == Lose = Pictures [loseText, loseText2,gameboy, loseText3, nameGame, nameGame2, nameGame3, ball, platform, wallsCollor, menu2, menu, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart, menuRestart2, nameBoy,nameBoy2,nameBoy3]
                    | view == Pause = Scale 0.50 0.50 $ Pictures [paused, paused2]
                    | otherwise = Pictures [ball, gameboy,nameGame, nameGame2, nameGame3, bricks, platform, wallsCollor, menu, menu2, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart, menuRestart2,nameBoy,nameBoy2,nameBoy3]
                      where
                        paused = Translate (-windowWidthFloat * 0.67) 50 $ Color yellow $ Text "PAUSED"
                        paused2 = Translate (-windowWidthFloat * 0.66) 50 $ Color yellow $ Text "PAUSED"


                        nameBoy = Scale 0.72 0.72 $ Rotate (-90) $ Translate (-380) (-420) $ Color cyan $ Text "GAMEBOY"
                        nameBoy2 =Scale 0.72 0.72 $ Rotate (-90) $ Translate (-384) (-424) $ Color azure $ Text "GAMEBOY"
                        nameBoy3 = Scale 0.72 0.72 $ Rotate (-90)  $ Translate (-387) (-424) $ Color magenta $ Text "GAMEBOY"


                        gameboy = Color azure (gameboys)
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

                          Rotate (-30) $ Translate (17) (401) (rectangleSolid windowWidthScore wallsWidth),
                          Rotate (-30) $ Translate (404) (174) (rectangleSolid windowWidthScore wallsWidth),
                          Rotate (-30) $ Translate (21) (-500) (rectangleSolid windowWidthScore wallsWidth),
                          Translate 18 328 (rectangleSolid windowWidthFloat wallsWidth), -- вверх 1 слой
                          Translate (-30) 328 (rectangleSolid windowWidthFloat wallsWidth), -- вверх 1 слой
                          Translate (225) 29 (rectangleSolid wallsWidth windowHeightFloat),  -- бок 1 слой
                          Translate (225) (-150) (rectangleSolid wallsWidth windowHeightFloat), -- бок слой 1 слой
                          Translate 27 (-448) (rectangleSolid windowWidthFloat wallsWidth), -- низ 1 слой
                          Translate (-28) (-448) (rectangleSolid windowWidthFloat wallsWidth), -- низ 1 слой
                          Translate (-225) 29 (rectangleSolid wallsWidth windowHeightFloat),  -- бок 1 слой
                          Translate (-225) (-150) (rectangleSolid wallsWidth windowHeightFloat),
                          Translate 107 378 (rectangleSolid windowWidthFloat wallsWidth), -- вверх 2 слой
                          Translate 50 378 (rectangleSolid windowWidthFloat wallsWidth),
                          Translate 309 80 (rectangleSolid wallsWidth windowHeightFloat),  -- бок 2 слой
                          Translate 309 (-97) (rectangleSolid wallsWidth windowHeightFloat)]

                        helloStr = Translate (-windowWidthFloat * 2.8) 350 $ Color yellow $ Text "Hello"

                        nameGame = Translate (-windowWidthFloat * 2.3 ) 370 $ Color azure $ Text "ARKANOID"
                        nameGame2 = Translate (-windowWidthFloat * 2.28 ) 374 $ Color magenta $ Text "ARKANOID"
                        nameGame3 = Translate (-windowWidthFloat * 2.29 ) 374 $ Color cyan $ Text "ARKANOID"
                        nameGame4 = Scale 2.3 2.3 $ Translate (-windowWidthFloat * 0.8 ) 310 $ Color azure $ Text "ARKANOID"
                        nameGame5 = Scale 2.3 2.3 $ Translate (-windowWidthFloat * 0.78 ) 314 $ Color magenta $ Text "ARKANOID"
                        nameGame6 = Scale 2.3 2.3 $ Translate (-windowWidthFloat * 0.79 ) 314 $ Color cyan $ Text "ARKANOID"

                        menu = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) 150 $ Color yellow $ Text "| Press 'M' - to enter MENU "
                        menu2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) 150 $ Color yellow $ Text "| Press 'M' - to enter MENU "
                        menuPaused = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) 0 $ Color yellow $ Text "| Press 'F' - to Paused     "
                        menuPausd2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) 0 $ Color yellow $ Text "| Press 'F' - to Paused  "
                        menuRestart = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) (-150) $ Color yellow $ Text "| Press 'R' - to Restart     "
                        menuRestart2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) (-150) $ Color yellow $ Text "| Press 'R' - to Restart  "
                        menuExit = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) (-300) $ Color yellow $ Text "| Press 'Esc' - to exit Game  "
                        menuExit2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) (-300) $ Color yellow $ Text "| Press 'Esc' - to exit Game  "

                        menuText =  Translate (-windowWidthFloat * 1.7) 370 $ Color yellow $ Text "Menu"
                        menuText2 =  Translate (-windowWidthFloat * 1.69) 370 $ Color yellow $ Text "Menu"
                        menuTextBonus =  Translate (-windowWidthFloat * 1.7) (-370) $ Color yellow $ Text "Bonus"
                        menuTextBonus2 =  Translate (-windowWidthFloat * 1.69) (-370) $ Color yellow $ Text "Bonus"

                        menuTextControl = Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 300 $ Color white $ Text "- control arrow  <- | ->"
                        menuTextRestrat = Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 150 $ Color white $ Text "- 'R' restart game"
                        menuTextPaused = Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 0 $ Color white $ Text "- 'F' paused game"
                        menuTextEsc = Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) (-150) $ Color white $ Text "- 'Esc' to exit game "
                        menuTextContine = Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) (-300) $ Color white $ Text "- Press 'Space' to play"
                        menuTextBon = Scale 0.65 0.70 $  Translate (-windowWidthFloat * 1.6) (-750) $ Color white $ Text "Hitting a corner of a brick - one shot it"

                        tutorialTextW = Translate (-windowWidthFloat * 2.5) 110 $ Color white $ Text "Welcome to the game ARKANOID!"
                        tutorialTextContinue = Translate (-windowWidthFloat * 1.5) (-100) $ Color white $ Text "Press 'Enter' to play"

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







-- Обрабатывает входящие события
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  |key == KeySpace && view == Menu = state {view = LevelView}
  |key == KeySpace && view == Pause = state{view = LevelView}
  |key == KeyEnter && view == StartScreen = state{view = Menu}   -- Handles continue while in start screen
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

helloStr :: String -> Picture
helloStr name = Translate (-650) 220 $ Scale 0.2 0.2 $ Color yellow $ Text ("Welcome " ++ name ++ " !")

-- Запустить игру
run :: IO()
run = do
  putStrLn "Hello, what's your name?"

  gen <- getStdGen
  play  window bgColor fps (initState (fst (randomR randRange gen ))  StartScreen ) draw eventHandler tick
