module ResultService where

import Graphics.Gloss.Interface.IO.Game
import System.Directory

import Constants
import Data.List
import Data.Fixed
import Data.Maybe (isNothing)


formatTime :: String -> String
formatTime (x:xs) | x == '.' = x:head xs:"s"
                  | otherwise = x:formatTime xs

formatResult :: String -> String
formatResult [] = ""
formatResult ('.':c:xs) = '.':c:formatResult (dropWhile (/='s') xs)
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
takeLast n x = drop (length x - n) x

getNamesFromString :: String -> [String]
getNamesFromString [] = []
getNamesFromString s = takeWhile (/= ' ') s : getNamesFromString (drop 1 (dropWhile (/= '\n') s))

set :: Eq a => [a] -> [a] -> [a]
set [] acc = acc
set (x:xs) acc
  | isNothing (find (==x) acc) = set xs (x : acc)
  | otherwise = set xs acc

getNames :: IO [String]
getNames = do
             file <- readFile resultsFilePath
             return (set (getNamesFromString file) [])

getProfileStr :: Int -> [String] -> String
getProfileStr 0 x = show 0 ++ " New profile\n" ++ getProfileStr 1 x
getProfileStr n [] = ""
getProfileStr n (x:xs) = show n ++ " " ++ x ++ "\n" ++ getProfileStr (n + 1) xs

getResultsByName :: String -> String -> [Pico]
getResultsByName _ [] = []
getResultsByName name file | name == currName = (read result::Pico): getResultsByName name tailLines
                           | otherwise = getResultsByName name tailLines
                              where
                                  currName = takeWhile (/=' ') file
                                  result = takeWhile (/='s') (drop 1 (dropWhile (/=' ') line))
                                  line = takeWhile (/='\n') file
                                  tailLines = drop 1 (dropWhile (/='\n') file)

getResultsAll :: String -> [Pico]
getResultsAll [] = []
getResultsAll file = (read result::Pico): getResultsAll tailLines
                      where
                        result = takeWhile (/='s') (drop 1 (dropWhile (/=' ') line))
                        line = takeWhile (/='\n') file
                        tailLines = drop 1 (dropWhile (/='\n') file)

getPersonalBest :: String -> IO Pico
getPersonalBest name = do
                        file <- readFile resultsFilePath
                        let results = getResultsByName name file
                        if not $ null results
                        then
                          return (minimum results)
                        else
                          return 0

getGlobalRecord :: IO Pico
getGlobalRecord = do
                     file <- readFile resultsFilePath
                     let results = getResultsAll file
                     if not $ null results
                     then
                      return (minimum results)
                     else
                      return 0