module Type (Mover(..), Env(..), startEnv) where

data Mover = Frogger { -- The centre position of the frog, in x
                       x :: Float
                       -- The centre position of the frog, in x
                     , y :: Float
                       -- The "size" of the frog (the length of one edge of the square)
                     , s :: Float
           }
           | Car { -- The centre position of the car in x
                   x :: Float
                   -- The centre position of the car in y
                 , y :: Float
                   -- The length of the car
                 , l :: Float
                   -- The width of the car
                 , w :: Float
                   -- The velocity of the car (+ve means going left-to-right)
                 , v :: Float
           } deriving Show

data Env = E { -- The Frogger
               player :: Mover
               -- The enemies
             , enemies :: [Mover]
         } deriving Show

startEnv :: Env
startEnv = E { player = Frogger {x = 310.0, y = 5.0, s = 20.0}
             , enemies = [Car {x = 0.0,   y = 34.0, l = 48.0, w = 24.0, v = 0.5}
                         ,Car {x = 640.0, y = 66.0, l = 48.0, w = 24.0, v = -0.5}
                         ,Car {x = 0.0,   y = 100.0, l = 48.0, w = 24.0, v = 0.5}
                         ,Car {x = 640.0, y = 132.0, l = 48.0, w = 24.0, v = -0.5}
                         ,Car {x = 0.0,   y = 164.0, l = 48.0, w = 24.0, v = 0.5}
                         ]
         }
