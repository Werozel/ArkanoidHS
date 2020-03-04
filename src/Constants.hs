module Constants where


-- Returns fps for the game
fps :: Int
fps = 60

-- Returns width for window
windowWidth :: Int
windowWidth = 600

-- Returns height for window
windowHeight :: Int
windowHeight = 400

-- Returns horizontal offset for window
windowOffsetX :: Int
windowOffsetX = 200

-- Returns vertical offset for window
windowOffsetY :: Int
windowOffsetY = 100

-- Returns grey coefficient for background color
bgGreyN :: Float
bgGreyN = 0.4

-- Returns range for ball horizontal direction random generator
randRange :: (Float, Float)
randRange = (-10, 10)

-- Returns initial ball vertical speed
initBallDirectionY :: Float
initBallDirectionY = -5

-- Returns initial ball vertical position
initBallPositionY :: Float
initBallPositionY = -150

-- Returns initial ball vertical position
initPlatformPositionY :: Float
initPlatformPositionY = -50

-- Returns ball radius
ballRadius :: Float
ballRadius = 10
