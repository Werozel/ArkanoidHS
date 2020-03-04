module Run
  ( run
  ) where

import Graphics.Gloss.Interface.Pure.Game

import Lib
import Constants
import Base

run :: IO()
run = play window bgColor fps initState draw eventHandler tick