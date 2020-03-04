module Run
  ( run
  ) where

import Graphics.Gloss.Interface.Pure.Game

import System.Random

import Lib
import Constants
import Base

run :: IO()
run =
  play window bgColor fps (initState rnd) draw eventHandler tick
    where
      gen = getStdGen
      -- FIXME do a random generator
      rnd = 0
