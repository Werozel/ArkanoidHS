module ResultService where

import Graphics.Gloss.Interface.IO.Game
import System.Directory


formatTime :: String -> String
formatTime (x:xs) | x == '.' = x:head xs:"s"
                  | otherwise = x:formatTime xs

formatResult :: String -> String
formatResult [] = ""
formatResult ('.':c:xs) = '.':c:formatResult (drop 11 xs)
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
takeLast n x = drop (length x - (n + 1)) x