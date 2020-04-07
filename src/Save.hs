{-# LANGUAGE RecordWildCards #-}
module Save where

import Lib
import Constants

import Data.Time.Clock
import System.IO
import System.Directory

saveResult :: GameState -> IO()
saveResult state@GameState {..} = if not isSaved 
                                  then 
                                    do
                                      let fpath = "saves/results.txt"
                                      appendFile fpath (name ++ " " ++ (show playTime) ++ "\n")
                                      putStrLn "Saved"
                                  else
                                    return ()
