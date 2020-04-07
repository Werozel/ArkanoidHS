{-# LANGUAGE RecordWildCards #-}
module Run
  ( run, tick, initState
  ) where

import Graphics.Gloss.Interface.IO.Game

import System.Random
import Data.List
import Data.Time.Clock
import System.IO
import System.Directory

import Lib
import Constants
import Base
import LevelGenerator
import DrawFunctions
import Save
import Data.Fixed
import Data.String (String)


--Возвращает начальное состояние игры
initState :: String -> Float -> View ->  IO GameState
initState name rnd v = return $ GameState name False (secondsToNominalDiffTime 0) False v initBallPos initBallDirection initPlatformPos 0 initGrid 3  NotFinished [NonePressed]
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
tick ::Float -> GameState -> IO GameState
tick s state@GameState{..} | view /= LevelView = return state
                           | result == Win =
                                  return $ GameState name True playTime False WinView ballPos (0, 0) platformPos level grid 0 Win [NonePressed]
                           | result == Lose = do
                              saveResult state
                              return $ GameState name isSaved playTime False LoseView ballPos (0, 0) platformPos level grid 0 Lose [NonePressed]
                           | otherwise = return $ GameState name isSaved (playTime + secondsToNominalDiffTime (realToFrac s)) isPlaying view newBallPos newBallDirection newPlatformPos level newGrid bricksLeftUpdated newResult keysPressed
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
                              saveCheck = saveResult state

formatTime :: String -> String
formatTime (x:xs) | x == '.' = x:head xs:"s"
                  | otherwise = x:formatTime xs

formatResult :: String -> String
formatResult [] = ""
formatResult ('.':c:xs) = '.':c:formatResult (drop 11 xs)
formatResult (x:xs) = x:formatResult xs

splitResults :: String -> [String]
splitResults s = splitResults2 s ""

splitResults2 :: String -> String -> [String]
splitResults2 [] acc = [acc]
splitResults2 (x:xs) acc | x == 's' = (acc++[x]):splitResults2 xs ""
                         | otherwise = splitResults2 xs (acc++[x])

getResultText :: String -> IO String
getResultText path = do
                       fileExists <- doesFileExist path
                       if fileExists
                       then do
                          res <- readFile path
                          if null res
                          then
                            return ""
                          else
                            return $ formatResult res
                       else
                          return ""

translateAllY :: Float -> [Picture] -> [Picture]
translateAllY _ [] = []
translateAllY start (x:xs) = Translate 0 start x : translateAllY (start - 80) xs

takeLast :: Int -> [a] -> [a]
takeLast n x = drop (length x - (n + 1)) x

-- Рисует картинку в окне для текущего состояния игры
draw :: GameState -> IO Picture
draw state@GameState {..} = do
                          currTime <- getCurrentTime
                          let
                            playTimeText = formatTime (show (nominalDiffTimeToSeconds playTime))
                            playTimePic = Scale 0.35 0.35 $ Translate (windowWidthFloat * 4.5) (-100) $ Color yellow $ Text playTimeText

                          resultText <- getResultText resultsFilePath
                          let results = drop 1 $ reverse $ takeLast 30 $ splitResults resultText
                              resultPics = map (Scale 0.5 0.5 . Color white . Text) results
                              resultPics10 = Translate (-960 + 130) 0 $ Pictures $ translateAllY (540 - 200) $ take 10 resultPics
                              resultPics20 = Translate (-250) 0 $ Pictures $ translateAllY (540 - 200) $ take 10 $ drop 10 resultPics
                              resultPics30 = Translate (960 - 500) 0 $ Pictures $ translateAllY (540 - 200) $ take 10 $ drop 20 resultPics
                              resultTextPic = Pictures [resultPics10, resultPics20, resultPics30]
                          case view of
                           StartScreen -> return (Pictures [ helloStrPic, tutorialTextW, tutorialTextContinue])
                           Menu -> return (Pictures [menuText,menuText2, menuTextControl, menuTextRestrat, menuTextResults, menuTextPaused,menuTextContine,menuTextEsc,menuTextBonus,menuTextBonus2,nameGame4,nameGame5,nameGame6, menuTextBon])
                           WinView -> return (Pictures [playTimePic, playerName, winText, menuSpace, menuSpace2, winText2, gameboy, winText3, nameGame,nameGame2, nameGame3, platform, wallsCollor, menu2, menu, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart ,menuRestart2, menuResults, menuResults2, nameBoy,nameBoy2,nameBoy3])
                           LoseView -> return (Pictures [playTimePic, playerName, loseText, menuSpace, menuSpace2, loseText2, gameboy, loseText3, nameGame, nameGame2, nameGame3, ball, platform, wallsCollor, menu2, menu, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart, menuRestart2, menuResults, menuResults2, nameBoy,nameBoy2,nameBoy3])
                           Pause -> return (Pictures [playTimePic, playerName, paused, menuSpace, menuSpace2, paused2, ball, gameboy,nameGame, nameGame2, nameGame3, bricks, platform, wallsCollor, menu, menu2, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart, menuRestart2, menuResults, menuResults2,nameBoy,nameBoy2,nameBoy3])
                           ResultsMenu -> return (Pictures[resultTitle, resultBorders, resultTextPic, resultMenuHint, resultMenuHint2])
                           LevelView -> return (Pictures [playTimePic, playerName, ball,menuSpace, menuSpace2, gameboy,nameGame, nameGame2, nameGame3, bricks, platform, wallsCollor, menu, menu2, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart, menuRestart2, menuResults, menuResults2,nameBoy,nameBoy2,nameBoy3])
                           _ -> return (Pictures [ball,menuSpace, menuSpace2, gameboy,nameGame, nameGame2, nameGame3, bricks, platform, wallsCollor, menu, menu2, menuPaused, menuPausd2 , menuExit, menuExit2, menuRestart, menuRestart2,menuResults, menuResults2,nameBoy,nameBoy2,nameBoy3])
                      where
                        resultTitle = Translate (-150) (windowHeightFloat - 150) $ Scale 0.8 0.8 $ Color white $ Text "Results"
                        resultBorders = Color yellow $ Pictures [resultBorderLeft, resultBorderRight, resultBorderTop, resultBorderBottom]
                        resultBorderLeft = Translate (-960 + 100) 0 $ Line [(0, 540 - 120), (0, -540 + 80)]
                        resultBorderRight = Translate (960 - 100) 0 $ Line [(0, 540 - 120), (0, -540 + 80)]
                        resultBorderTop = Translate 0 (540 - 120) $ Line [(-960 + 100, 0), (960 - 100, 0)]
                        resultBorderBottom = Translate 0 (-540 + 80) $ Line [(-960 + 100, 0), (960 - 100, 0)]

                        resultMenuHint = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) (4 * (windowHeightFloat - 150)) $ Color yellow $ Text " Press 'M' - to enter MENU "
                        resultMenuHint2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) (4 * (windowHeightFloat - 150)) $ Color yellow $ Text " Press 'M' - to enter MENU "

                        playerName = Scale 0.55 0.55 $ Translate (windowWidthFloat * 2.72) 100 $ Color white $ Text name



                        paused = Scale 0.35 0.35 $ Translate (-windowWidthFloat * 0.67) 0 $ Color yellow $ Text "PAUSED"
                        paused2 = Scale 0.35 0.35 $  Translate (-windowWidthFloat * 0.66) 0 $ Color yellow $ Text "PAUSED"

                        nameBoy = Scale 0.72 0.72 $ Rotate (-90) $ Translate (-380) (-420) $ Color cyan $ Text "GAMEBOY"
                        nameBoy2 =Scale 0.72 0.72 $ Rotate (-90) $ Translate (-384) (-424) $ Color azure $ Text "GAMEBOY"
                        nameBoy3 = Scale 0.72 0.72 $ Rotate (-90)  $ Translate (-387) (-424) $ Color magenta $ Text "GAMEBOY"

                        gameboy = Color azure gameboys
                        gameboys = Pictures [  Translate 110 (-400)  $ Color (dark chartreuse) (circleSolid 22),
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

                        helloStrPic = Translate (-windowWidthFloat * 2.8) 350 $ Color yellow $ helloStr name

                        nameGame = Translate (-windowWidthFloat * 2.3 ) 370 $ Color azure $ Text "ARKANOID"
                        nameGame2 = Translate (-windowWidthFloat * 2.28 ) 374 $ Color magenta $ Text "ARKANOID"
                        nameGame3 = Translate (-windowWidthFloat * 2.29 ) 374 $ Color cyan $ Text "ARKANOID"
                        nameGame4 = Scale 2.3 2.3 $ Translate (-windowWidthFloat * 0.8 ) 310 $ Color azure $ Text "ARKANOID"
                        nameGame5 = Scale 2.3 2.3 $ Translate (-windowWidthFloat * 0.78 ) 314 $ Color magenta $ Text "ARKANOID"
                        nameGame6 = Scale 2.3 2.3 $ Translate (-windowWidthFloat * 0.79 ) 314 $ Color cyan $ Text "ARKANOID"

                        menu = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) 150 $ Color yellow $ Text "| Press 'M' - to enter MENU "
                        menu2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) 150 $ Color yellow $ Text "| Press 'M' - to enter MENU "
                        menuPaused = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) 0 $ Color yellow $ Text "| Press 'F' - to Pause     "
                        menuPausd2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) 0 $ Color yellow $ Text "| Press 'F' - to Pause  "
                        menuRestart = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) (-150) $ Color yellow $ Text "| Press 'R' - to Restart     "
                        menuRestart2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) (-150) $ Color yellow $ Text "| Press 'R' - to Restart  "
                        menuExit = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) (-300) $ Color yellow $ Text "| Press 'Esc' - to exit Game  "
                        menuExit2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) (-300) $ Color yellow $ Text "| Press 'Esc' - to exit Game  "
                        menuSpace = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) (-450) $ Color yellow $ Text "| Press 'Space' - to Continue  "
                        menuSpace2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) (-450) $ Color yellow $ Text "| Press 'Space' - to Continue  "
                        menuResults = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.8) (-600) $ Color yellow $ Text "| Press 'V' - for Results  "
                        menuResults2 = Scale 0.25 0.25 $ Translate (-windowWidthFloat * 8.809) (-600) $ Color yellow $ Text "| Press 'V' - for Results   "

                        menuText =  Scale 0.45 0.45 $ Translate (-windowWidthFloat * 1.7) 370 $ Color yellow $ Text "Menu"
                        menuText2 =  Scale 0.45 0.45 $ Translate (-windowWidthFloat * 1.69) 370 $ Color yellow $ Text "Menu"
                        menuTextBonus =  Scale 0.45 0.45 $ Translate (-windowWidthFloat * 1.7) (-430) $ Color yellow $ Text "Bonus"
                        menuTextBonus2 =  Scale 0.45 0.45 $ Translate (-windowWidthFloat * 1.69) (-430) $ Color yellow $ Text "Bonus"

                        menuTextControl = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 300 $ Color white $ Text "- control with arrows  <- | ->"
                        menuTextRestrat = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 150 $ Color white $ Text "- 'R' restarts the game"
                        menuTextPaused = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) 0 $ Color white $ Text "- 'F' pauses the game"
                        menuTextEsc = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) (-150) $ Color white $ Text "- 'Esc' to exit game "
                        menuTextContine = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) (-300) $ Color white $ Text "- Press 'Space' to play"
                        menuTextResults = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidthFloat * 1.6) (-450) $ Color white $ Text "- Press 'V' for results"

                        menuTextBon = Scale 0.45 0.45 $ Scale 0.65 0.70 $  Translate (-windowWidthFloat * 1.6) (-850) $ Color white $ Text "Hitting a corner of a brick - one shot it"

                        tutorialTextW = Scale 0.33 0.35 $ Translate (-windowWidthFloat * 2.5) 110 $ Color white $ Text "Welcome to the game ARKANOID!"
                        tutorialTextContinue = Scale 0.33 0.35 $ Translate (-windowWidthFloat * 1.5) (-100) $ Color white $ Text "Press 'Enter' to play"

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
eventHandler :: Event -> GameState -> IO GameState
eventHandler (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  | key == KeySpace && view == Menu = return (state {view = LevelView})
  | key == KeySpace && view == Pause = return (state {view = LevelView})
  | key == KeyEnter && view == StartScreen = return (state {view = Menu})
  | key == KeyLeft =
    return
      (state
         { keysPressed =
             if keyState == Down
               then LeftPressed : keysPressed
               else delete LeftPressed keysPressed
         })
  | key == KeyRight =
    return
      (state
         { keysPressed =
             if keyState == Down
               then RightPressed : keysPressed
               else delete RightPressed keysPressed
         })
  | otherwise = return state
eventHandler (EventKey (Char c) Down _ _ ) state@GameState{..}
  | c == 'm' = return state { view = Menu}
  | c == 'f' = return state { view = Pause}
  | c == 'r' = initState name 25 LevelView    -- Handles restart button
  | c == 'v' = return state { view = ResultsMenu}
  | otherwise = return state
eventHandler _ state = return state


helloStr :: String -> Picture
helloStr name = Translate (-650) 220 $ Scale 0.2 0.2 $ Color yellow $ Text ("Welcome " ++ name ++ " !")


getName :: IO String
getName = do
  putStrLn "Введите имя: "
  name <- getLine
  if ' ' `elem` name
    then do
      putStrLn "Имя не должно содержать пробелы!"
      getName
    else return name

-- Запустить игру
run :: IO()
run = do
  gen <- getStdGen
  name <- getName

  init <- initState name (fst (randomR randRange gen )) StartScreen
  playIO window bgColor fps init draw eventHandler tick
