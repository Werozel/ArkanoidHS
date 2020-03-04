module Run
  ( run
  ) where

import Graphics.Gloss.Interface.Pure.Game

import System.Random

import Data.Time.Clock

import Lib
import Constants
import Base


run :: IO()
run = do
  gen <- getStdGen
  play window bgColor fps (initState (fst (randomR randRange gen))) draw eventHandler tick

