{-# LANGUAGE RecordWildCards #-}
module Save where

import Lib
import Constants

import Data.Time.Clock
import System.IO

saveResult :: GameState -> IO()
saveResult state@GameState {..} = if not isSaved 
                                  then 
                                    do
                                      endTime <- getCurrentTime
                                      let fpath = "saves/results.txt"
                                      appendFile fpath (name ++ " " ++ (show (diffUTCTime endTime startTime)) ++ "\n")
                                      putStrLn "Saved"
                                  else
                                    return ()

