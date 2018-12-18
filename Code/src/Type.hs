module Type (Mover(..), Env(..), emptyEnv) where

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

emptyEnv :: Env
emptyEnv = E { player = Frogger {x = 320.0, y = 240.0, s = 10.0}
             , enemies = [Car {x = 0.0, y = 120.0, l = 20.0, w = 10.0, v = 1}
                         ,Car {x = 640.0, y = 360.0, l = 20.0, w = 10.0, v = -1}]
         }
