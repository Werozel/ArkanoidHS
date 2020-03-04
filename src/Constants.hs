module Constants where


-- Returns fps for the game
fps :: Int
fps = 60

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

-- Returns range for ball horizontal direction random generator
randRange :: (Float, Float)
randRange = (-2 * speed, 2 * speed)

-- Returns initial ball vertical speed
initBallDirectionY :: Float
initBallDirectionY = 1 * speed

-- Returns initial ball vertical position
initBallPositionY :: Float
initBallPositionY = -150

-- Returns initial ball vertical position
initPlatformPositionY :: Float
initPlatformPositionY = -50

-- Returns ball radius
ballRadius :: Float
ballRadius = 6
