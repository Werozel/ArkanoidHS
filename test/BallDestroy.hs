{-# LANGUAGE RecordWildCards #-}

module BallDestroy where

import Graphics.Gloss.Interface.Pure.Game
import Lib
--import Graphics.Gloss.Data.Picture

setPosition :: GameState -> GameState
setPosition state@GameState {..} = do
  let blockPosition = position (head (head (bricks grid)))
  state {ballPos = blockPosition}

