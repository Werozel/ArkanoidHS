{-# LANGUAGE RecordWildCards #-}
module Save where

import Lib
import Constants

import Data.Time.Clock
import System.IO
import System.Directory

createFile :: String -> IO()
createFile path = do
  createDirectoryIfMissing False "saves"
  let path = resultsFilePath
  appendFile path ""
  return()

saveResult :: GameState -> IO()
saveResult state@GameState {..} = if not isSaved 
                                  then 
                                    do
                                      createDirectoryIfMissing False "saves"
                                      let fpath = resultsFilePath
                                      appendFile fpath (name ++ " " ++ (show playTime) ++ "\n")
                                  else
                                    return ()
