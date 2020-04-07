module Lib where

import Graphics.Gloss.Interface.Pure.Game

import Data.Time.Clock

import System.Random

import Constants

-- Кирпичная структура данных
data Brick = Brick {
  position :: Point,
  size :: Point,
  hitsLeft :: Int
} | NoBrick


-- виды ударов по мячу
data Hit = LeftHit | TopHit | RightHit | BottomHit |
           LeftTopHit | RightTopHit | LeftBottomHit | RightBottomHit |
           NoHit | PlatformHit deriving Eq
-- Одна строка из сетки
type BricksGridRow = [Brick]
-- All bricks
data BricksGrid = BricksGrid {
  bricks :: [BricksGridRow],
  lastHit :: Hit
}

-- Result of a game
data Result = Win | Lose | NoTime | NotFinished deriving Eq
-- Текущее меню или уровень
data View = MainMenu | ResultsMenu | SettingsMenu | LevelView | StartScreen | Pause | Menu | WinView | LoseView deriving Eq

data KeyPressed = LeftPressed | RightPressed | NonePressed deriving Eq
type KeysPressed = [KeyPressed]

-- Структура данных о состоянии игры
-- Point = (Float, Float)
data GameState = GameState {
  name :: String,   -- имя текущего игрока
  isSaved :: Bool, -- флаг указываеющий был ли сохранен текущий результат
  playTime :: NominalDiffTime,  -- время, которое длится текущий уровень
  isPlaying :: Bool, --  флаг указывает на то, что игра находится в процессе ожидания запуска
  view :: View, -- указывает, в каком состоянии находится окно
  ballPos :: Point, -- текущее положение шара
  ballDirection :: Vector, -- Vector = Point;текущее направление шара
  platformPos :: Point, -- текущее положение платформы
  level :: Int, -- Current level (if playing, else = 0)
  grid :: BricksGrid, -- расположение кирпичей
  bricksLeft :: Int, -- кирпичи, оставленные для удаления
  result :: Result, -- Результат игры или Неоконченный флаг
  keysPressed :: KeysPressed-- Key that is pressed at the momen

}



-- Возвращает цвет фона для окна
bgColor :: Color
bgColor = black

-- Окно для игры
window :: Display
window = FullScreen

-- Преобразует Hit на строку для функции Show
showHit :: Hit -> String
showHit TopHit = "TopHit"
showHit BottomHit = "BottomHit"
showHit LeftHit = "LeftHit"
showHit RightHit = "RightHit"
showHit LeftTopHit = "LeftTopHit"
showHit RightTopHit = "RightTopHit"
showHit LeftBottomHit = "LeftBottomHit"
showHit RightBottomHit = "RightBottomHit"
showHit PlatformHit = "PlatfromHit"
showHit _ = "UnknownHit"
