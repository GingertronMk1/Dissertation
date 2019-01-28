module Type ( RoadMover(..)
            , RiverMover(..)
            , Frogger(..)
            , Goal (..)
            , Env(..)
            , GameState(..)
            , startEnv
            , Drawable(..)
            , initSizeX
            , initSizeY
            , lanes
            ) where

import Graphics.UI.GLUT

-- TYPE DECLARATIONS

data Frogger = Frogger { -- The position of the frog, in x
                         f_X :: Float
                        -- The position of the frog, in x
                       , f_Y :: Float
                        -- The "size" of the frog (the length of one edge of the square)
                       , f_S :: Float
                        -- The velocity of the Frogger when moving on the back of a RiverObject
                       , f_V :: Float
               }
               deriving Show

data RoadMover = Car { -- The position of the car in x
                   ro_X :: Float
                   -- The position of the car in y
                 , ro_Y :: Float
                   -- The length of the car
                 , ro_L :: Float
                   -- The width of the car
                 , ro_W :: Float
                   -- The velocity of the car (+ve means going left-to-right)
                 , ro_V :: Float
           }
           deriving Show

data RiverMover = Croc { -- The position of the croc in x
                         ri_X :: Float
                         -- The position of the croc in y
                       , ri_Y :: Float
                         -- The length of the croc
                       , ri_L :: Float
                         -- The width of the croc
                       , ri_W :: Float
                         -- Velocity in x (+ve means going left-to-right)
                       , ri_V :: Float
                }
                | Turtles { -- Position in x
                            ri_X :: Float
                            -- Position in y
                          , ri_Y :: Float
                            -- Length
                          , ri_L :: Float
                            -- Width
                          , ri_W :: Float
                            -- Velocity in x
                          , ri_V :: Float
                            -- Are the turtles above water?
                          , aboveWater :: Bool
                }
                | Log { -- Position in x
                        ri_X :: Float
                        -- Position in y
                      , ri_Y :: Float
                        -- Length
                      , ri_L :: Float
                        -- Width
                      , ri_W :: Float
                        -- Velocity in x
                      , ri_V :: Float
                }
                deriving Show

data Goal =  Goal { -- Position in x
                    g_X :: Float
                  -- Position in y
                  , g_Y :: Float
                  -- Size (length of one edge of the square)
                  , g_S :: Float
          }
          deriving Show

data Env = E { -- The Frogger
               player :: Frogger
               -- The enemies on the road
             , roadEnemies :: [RoadMover]
               -- The "enemies" on the river
             , riverEnemies :: [RiverMover]
               -- The goal/s
             , goals :: [Goal]
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

data GameState = PreStart
               | Playing
               | Paused
               | PlayerDead String
               | LevelComplete
               deriving (Eq, Show)

-- CLASS DECLARATIONS

class Drawable a where
  -- A function to draw the object to the screen
  draw :: a -> IO()
  -- A function to update the object over time
  update :: a -> a
  -- A function to draw the object, preserving the current transformation matrix
  preservingDraw :: a -> IO()
  preservingDraw = preservingMatrix . draw
  -- Preserved drawing multiple items in a list
  preservingDraws :: [a] -> IO[()]
  preservingDraws = sequence . map preservingDraw

-- TYPE "CONSTRUCTORS"

newCar :: Float -> Float -> Float -> RoadMover
newCar nx ny nv = Car {ro_X = nx, ro_Y = ny, ro_V = nv, ro_L = 48.0 * signum nv, ro_W = 24.0}

newCroc :: Float -> Float -> Float -> RiverMover
newCroc nx ny nv = Croc {ri_X = nx, ri_Y = ny, ri_V = nv, ri_L = 72.0 * signum nv, ri_W = 24.0}

newTurtles :: Float -> Float -> Float -> RiverMover
newTurtles nx ny nv = Turtles {ri_X = nx, ri_Y = ny, ri_V = nv, ri_L = 72.0 * signum nv, ri_W = 24.0, aboveWater = True}

newLog :: Float -> Float -> Float -> RiverMover
newLog nx ny nv = Log {ri_X = nx, ri_Y = ny, ri_V = nv, ri_L = 48.0 * signum nv, ri_W = 24.0}

newGoal :: Float -> Float -> Goal
newGoal gx gy = Goal {g_X = gx, g_Y = gy, g_S = 24}

startEnv :: Env
startEnv = E { player = Frogger {f_X = 310.0, f_Y = 4.0, f_S = 20.0, f_V = 0.0}
             , roadEnemies = [newCar 0.0 34.0 0.5
                             ,newCar 640.0 66.0 (-0.5)
                             ,newCar 0.0 98.0 0.5
                             ,newCar 640.0 132.0 (-0.5)
                             ,newCar 0.0 164.0 0.5
                             ]
             , riverEnemies = [newCroc 0.0 228.0 0.5
                              ,newTurtles 640.0 260.0 (-0.5)
                              ,newLog 0.0 292.0 0.5
                              ]
             , goals = [newGoal 308.0 196.0]
             , frames = 0
             , time = 0
             , gameState = PreStart --Paused
             , gameScore = 0
         }

-- CLASS INSTANCE DECLARATIONS

instance Drawable RiverMover where
  update c@(Croc {ri_X = cx, ri_V = cv})   = c {ri_X = cx + cv}
  update t@(Turtles{ri_X = tx, ri_V = tv}) = t {ri_X = tx + tv}
  update l@(Log{ri_X = lx, ri_V = lv})     = l {ri_X = lx + lv}
  draw Croc {ri_X = cx, ri_Y = cy, ri_L = cl, ri_W = cw, ri_V = cv} 
    = do color $ Color3 0.0 0.5 (0.0 :: Float)
         translate $ Vector3 cx cy 0.0
         scale cl cw 1.0
         unitSquare
         color $ Color3 1.0 1.0 (1.0 :: Float)
         preservingMatrix $ do translate $ Vector3 0.8 (0.1) (0.0 :: Float)
                               scale 0.1 0.2 (1.0 :: Float)
                               unitSquare
                               translate $ Vector3 0.0 3.0 (0.0 :: Float)
                               unitSquare
  draw Turtles {ri_X = tx, ri_Y = ty, ri_L = tl, ri_W = tw, ri_V = tv} 
    = do translate $ Vector3 tx ty 0.0
         scale (tw * signum tv) tw 1.0
         drawTurtles
      where drawTurtles = do drawTurtle
                             translate $ Vector3 1.0 0.0 (0.0 :: Float)
                             drawTurtle
                             translate $ Vector3 1.0 0.0 (0.0 :: Float)
                             drawTurtle
            drawTurtle = do preservingMatrix $ do translate $ Vector3 0.1 0.1 (0.0 :: Float)
                                                  scale 0.8 0.8 (1.0 :: Float)
                                                  color $ Color3 0.0 1.0 (0.0 :: Float)
                                                  unitSquare
                                                  color $ Color3 1.0 0.6 (0.0 :: Float)
                                                  unitCircle

  draw Log {ri_X = lx, ri_Y = ly, ri_L = ll, ri_W = lw, ri_V = lv} 
    = do color $ Color3 0.6 0.3 (0.2 :: Float)
         translate $ Vector3 lx ly 0.0
         scale ll lw 1.0
         unitSquare

instance Drawable Goal where
  draw Goal {g_X = gx, g_Y = gy, g_S = gs} 
    = do color $ Color3 0.8 0.7 (0.2 :: Float)
         translate $ Vector3 gx gy 0.0
         scale gs gs 1.0
         unitSquare
  update = id

instance Drawable Frogger where
  draw Frogger {f_X = fx, f_Y = fy, f_S = fs} = do color $ Color3 0.0 1.0 (0.0 :: Float)
                                                   translate $ Vector3 fx fy 0.0
                                                   scale fs fs 1.0
                                                   unitSquare
  update (f@Frogger {f_X = fx, f_V = fv}) = f {f_X = fx + fv}

instance Drawable RoadMover where
  draw Car {ro_X = cx, ro_Y = cy, ro_L = cl, ro_W = cw} = do color $ Color3 1.0 0.0 (0.0 :: Float)
                                                             translate $ Vector3 cx cy 0.0
                                                             scale cl cw 1.0
                                                             unitSquare
                                                             color $ Color3 0.3 0.3 (1.0 :: Float)
                                                             translate $ Vector3 0.8 0.0 (0.0 :: Float)
                                                             scale 0.1 1.0 (1.0 :: Float)
                                                             unitSquare
  update c@(Car {ro_X = cx, ro_V = cv}) = c {ro_X = cx + cv}



-- ADDITIONAL HELPERS FOR DRAWING

unitCircle :: IO()
unitCircle = let n = 50.0
                 points = [(0.5*(sin (2*pi*k/n)+1.0), 0.5*(cos (2*pi*k/n)+1.0), 0) | k <- [1..n]]
              in (renderPrimitive Polygon . mapM_ makeVertex) points

unitSquare :: IO()
unitSquare = let us = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float, Float, Float)]
             in (renderPrimitive Quads . mapM_ makeVertex) us


makeVertex :: (Float, Float, Float) -> IO()
makeVertex (x,y,z) = vertex $ Vertex3 x y z

-- INITIAL VALUES FOR THINGS

initSizeX :: Float
initSizeX = 640

initSizeY :: Float
initSizeY = 480

lanes :: [Float]
lanes = let lane = realToFrac (initSizeY / 15) :: Float
         in tail [0.0,lane..]
