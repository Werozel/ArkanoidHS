module Test where

import Lib

data Test = Test {
  name :: String, -- Название теста
  state :: GameState, -- Состояние игры
  tick :: Float -> GameState -> IO GameState, -- Функция tick из playIO
  tickCount :: Int, -- Номер текущего тика
  tickLimit :: Int, -- Ограничение на количество тиков, после которого тест будет признан провальным
  testTick :: GameState -> IO GameState, -- Функция, которая выполняется после каждого тика
  waitAll :: Bool, -- Нужно ли обязательно ждать все tickLimit тиков
  checkSuccess :: Test -> Bool -- Функция, которая проверяет, не выполнился ли тес,
}