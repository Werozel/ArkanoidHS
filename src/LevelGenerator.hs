module LevelGenerator where

import Lib
import Graphics.Gloss.Data.Picture
import Constants

generateLevel :: Int -> BricksGrid
generateLevel difficult = BricksGrid
                  [[Brick (-150, 100) (brickLength, brickHeight) 3,
<<<<<<< HEAD
                   Brick (0, 100.0) (brickLength, brickHeight) 3,
                   Brick (150, 100.0) (brickLength, brickHeight) 3,
                   Brick (0, 150.0 )(brickLength, brickHeight) 3,
                   Brick (-100, 150.0 )(brickLength, brickHeight) 3,
                   Brick (100, 150.0 )(brickLength, brickHeight) 3
=======
                   Brick (-90, 100.0) (brickLength, brickHeight) 3,
                   Brick (-30, 100.0) (brickLength, brickHeight) 3,
                   Brick (30, 100.0) (brickLength, brickHeight) 3,
                   Brick (90, 100.0) (brickLength, brickHeight) 3,
                   Brick (150, 100.0) (brickLength, brickHeight) 3
                  ],
                  [Brick (-150, 120) (brickLength, brickHeight) 3,
                   Brick (-90, 120.0) (brickLength, brickHeight) 3,
                   Brick (-30, 120.0) (brickLength, brickHeight) 3,
                   Brick (30, 120.0) (brickLength, brickHeight) 3,
                   Brick (90, 120.0) (brickLength, brickHeight) 3,
                   Brick (150, 120.0) (brickLength, brickHeight) 3
                  ],
                  [Brick (-150, 150) (brickLength, brickHeight) 3,
                   Brick (-90, 150.0) (brickLength, brickHeight) 3,
                   Brick (-30, 150.0) (brickLength, brickHeight) 3,
                   Brick (30, 150.0) (brickLength, brickHeight) 3,
                   Brick (90, 150.0) (brickLength, brickHeight) 3,
                   Brick (150, 150.0) (brickLength, brickHeight) 3
                  ],
                  [Brick (-150, 170) (brickLength, brickHeight) 3,
                   Brick (-90, 170.0) (brickLength, brickHeight) 3,
                   Brick (-30, 170.0) (brickLength, brickHeight) 3,
                   Brick (30, 170.0) (brickLength, brickHeight) 3,
                   Brick (90, 170.0) (brickLength, brickHeight) 3,
                   Brick (150, 170.0) (brickLength, brickHeight) 3
>>>>>>> 03c12f71528169c2dd4aee4d73324bb9676e119d
                  ]] NoHit
