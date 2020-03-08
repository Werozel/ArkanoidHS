module Constants where

import Graphics.Gloss.Interface.Pure.Game

-- Returns fps for the game
fps :: Int
fps = 1

-- Returns width for window
windowWidthFloat :: Float
windowWidthFloat = 400

windowWidth :: Int
windowWidth = 400

-- Returns height for window
windowHeightFloat :: Float
windowHeightFloat = 600

windowHeight :: Int
windowHeight = 600

-- Returns horizontal offset for window
windowOffsetX :: Int
windowOffsetX = 200

-- Returns vertical offset for window
windowOffsetY :: Int
windowOffsetY = 100

-- Returns grey coefficient for background color
bgGreyN :: Float
bgGreyN = 0.4

-- Returns speed coefficient for the ball
speed :: Float
speed = 3

-- Returns speed coefficient for the platform
platformSpeed :: Float
platformSpeed = 200

-- Returns range for ball horizontal direction random generator
randRange :: (Float, Float)
randRange = (-2 * speed, 2 * speed)

-- Returns initial ball vertical speed
initBallDirectionY :: Float
initBallDirectionY = 1 * speed

-- Returns initial ball vertical position
initBallPositionY :: Float
initBallPositionY = 130

-- Returns initial ball vertical position
initPlatformPositionY :: Float
initPlatformPositionY = -250

-- Returns platform length
platformLength :: Float
platformLength = 100

-- Returns platform height
platformHeight :: Float
platformHeight = 10

-- Returns walls width
wallsWidth :: Float
wallsWidth = 5

-- Returns ball radius
ballRadius :: Float
ballRadius = 6

-- Returns brick height
brickHeight :: Float
brickHeight = 25

-- Returns brick length
brickLength :: Float
brickLength = 70

-- Returns single hit count
singleHit :: Int
singleHit = 1

