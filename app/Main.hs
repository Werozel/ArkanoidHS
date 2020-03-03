module Main where
import Graphics.Gloss

import Lib

main :: IO ()
main = display getWindow white getElements


getWindow :: Display
getWindow = InWindow "Arcanoid" (300, 300) (20, 20)


getCircle :: Float -> Picture
getCircle x = ThickCircle (5*x) 5


getElements :: Picture
getElements = Pictures [circleSolid 5, translate 0 (-50) (rectangleSolid 20 5)]