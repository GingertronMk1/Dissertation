module Type (Mover(..), Env(..), GameState(..), startEnv) where

data Mover = Frogger { -- The position of the frog, in x
                       x :: Float
                       -- The position of the frog, in x
                     , y :: Float
                       -- The "size" of the frog (the length of one edge of the square)
                     , s :: Float
           }
           | Car { -- The position of the car in x
                   x :: Float
                   -- The position of the car in y
                 , y :: Float
                   -- The length of the car
                 , l :: Float
                   -- The width of the car
                 , w :: Float
                   -- The velocity of the car (+ve means going left-to-right)
                 , v :: Float
           }
           | Croc { -- The position of the croc in x
                    x :: Float
                    -- The position of the croc in y
                  , y :: Float
                    -- The length of the croc
                  , l :: Float
                    -- The width of the croc
                  , w :: Float
                    -- Velocity in x (+ve means going left-to-right)
                  , v :: Float
           }
           | Turtles { -- Position in x
                       x :: Float
                       -- Position in y
                     , y :: Float
                       -- Length
                     , l :: Float
                       -- Width
                     , w :: Float
                       -- Velocity in x
                     , v :: Float
                       -- Are the turtles above water?
                     , aboveWater :: Bool
           }
           | Log { -- Position in x
                   x :: Float
                   -- Position in y
                 , y :: Float
                   -- Length
                 , l :: Float
                   -- Width
                 , w :: Float
                   -- Velocity in x
                 , v :: Float
           }
           deriving Show

data Env = E { -- The Frogger
               player :: Mover
               -- The enemies
             , enemies :: [Mover]
               -- The total elapsed number of frames
             , frames :: Int
               -- The total elapsed time since game start
             , time :: Int
               -- The current state of the game
             , gameState :: GameState
               -- The current score
             , gameScore :: Int
         }
         deriving Show

data GameState = PreStart | Playing | Paused | PlayerDead
                 deriving (Eq, Show)

newCar :: Float -> Float -> Float -> Mover
newCar nx ny nv = Car {x = nx, y = ny, v = nv, l = 48.0 * signum nv, w = 24.0}

newCroc :: Float -> Float -> Float -> Mover
newCroc nx ny nv = Croc {x = nx, y = ny, v = nv, l = 72.0 * signum nv, w = 24.0}

newTurtles :: Float -> Float -> Float -> Mover
newTurtles nx ny nv = Turtles {x = nx, y = ny, v = nv, l = 72.0 * signum nv, w = 24.0, aboveWater = True}

newLog :: Float -> Float -> Float -> Mover
newLog nx ny nv = Log {x = nx, y = ny, v = nv, l = 48.0 * signum nv, w = 24.0}

startEnv :: Env
startEnv = E { player = Frogger {x = 310.0, y = 4.0, s = 20.0}
             , enemies = [newCar 0.0 34.0 0.5
                         ,newCar 640.0 66.0 (-0.5)
                         ,newCar 0.0 98.0 0.5
                         ,newCar 640.0 132.0 (-0.5)
                         ,newCar 0.0 164.0 0.5
                         ,newCroc 0.0 228.0 0.5
                         ,newTurtles 640.0 260.0 (-0.5)
                         ,newLog 0.0 292.0 0.5
                         ]
             , frames = 0
             , time = 0
             , gameState = Paused
             , gameScore = 0
         }
