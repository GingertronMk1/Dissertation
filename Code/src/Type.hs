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
            , newPlayer
            ) where

import Graphics.UI.GLUT

-- TYPE DECLARATIONS

type Lane = Int

-- A data type to be contained by all drawn objects
-- Cuts down on boilerplate x, y, v, l, w values all over the place
data Entity = Entity { -- Position in x
                       x :: Float
                       -- Position in y
                     , y :: Float
                       -- Velocity in x
                     , dX :: Float
                       -- Velocity in y
                     , dY :: Float
                       -- Length
                     , l  :: Float
                       -- Width
                     , w  :: Float
              }
              deriving (Eq, Show)

data Frogger = Frogger { -- The Entity containing important values about the Frogger
                         fr_Entity :: Entity
               }
               deriving (Eq, Show)

data RoadMover = Car { -- The Entity containing important values about the Car
                       ro_Entity :: Entity
                 }
                 deriving (Eq, Show)

data RiverMover = Croc { -- The Entity containing important values about the Croc
                         ri_Entity :: Entity
                  }
                | Turtles {  -- Are the turtles above water?
                            aboveWater :: Bool
                             -- The Entity containing important values about the Turtles
                          , ri_Entity :: Entity
                }
                | Log { -- The Entity containing important values about the Log
                        ri_Entity :: Entity
                }
                deriving (Eq, Show)

data Goal =  Goal { -- The Entity containing important values about the Goal
                    go_Entity :: Entity
                    -- Does the goal currently have a Frogger on it?
                  , is_Occupied :: Bool
          }
          deriving (Eq, Show)

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
             , time :: Float
               -- The current state of the game
             , gameState :: GameState
               -- The current score
             , gameScore :: Int
               -- The current level
             , level :: Int
         }
         deriving (Eq, Show)

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
  -- A function to draw the object, preserving the current transformation matrix
  preservingDraw :: a -> IO()
  preservingDraw = preservingMatrix . draw
  -- Preserved drawing multiple items in a list
  preservingDraws :: [a] -> IO[()]
  preservingDraws = sequence . map preservingDraw
  -- Get the Entity out of the object
  getEntity :: a -> Entity
  -- Get the x value out of that entity
  getX :: a -> Float
  getX = x . getEntity
  -- Get the y value out of that entity
  getY :: a -> Float
  getY = y . getEntity
  -- Get the dX value out of that entity
  getdX :: a -> Float
  getdX = dX . getEntity
  -- Get the dY value out of that entity
  getdY :: a -> Float
  getdY = dY . getEntity
  -- Get the l value out of that entity
  getL :: a -> Float
  getL = l . getEntity
  -- Get the w value out of that entity
  getW :: a -> Float
  getW = w . getEntity
  -- Update values in entities
  updateX :: a -> a
  updateX d = setX (getX d + getdX d) d
  updateY :: a -> a
  updateY d = setY (getY d + getdY d) d
  -- Set values in entities
  setX :: Float -> a -> a
  setY :: Float -> a -> a
  setdX :: Float -> a -> a
  setdY :: Float -> a -> a
  -- A function to update the object over time
  update :: a -> a
  update = updateX . updateY

-- TYPE "CONSTRUCTORS"

newFrogger :: Frogger
newFrogger = Frogger {fr_Entity = Entity { dX = 0
                                         , dY = 0
                                         , l = 20
                                         , w = 20}
                     }

newPlayer :: Frogger
newPlayer = setY 4.0 . (\s -> setX ((initSizeX - getW s)/2) s) $ newFrogger

newCar :: Lane -> [Float] -> RoadMover
newCar l v = let x = case mod l 2 of 0 -> 0.0
                                     1 -> initSizeX
                 nv = v !! l
              in Car {
                     ro_Entity = Entity {x = x
                                         ,y = lanes !! l + 2
                                         ,dX = nv
                                         ,dY = 0
                                         ,l = 48.0 * signum nv
                                         ,w = 24.0
                                         }
                     }

newCroc :: Lane -> [Float] -> RiverMover
newCroc l v = let x = case mod l 2 of 0 -> 0.0
                                      1 -> initSizeX
                  nv = v !! l
               in Croc {
                       ri_Entity = Entity {x = x
                                           ,y = lanes !! l + 2
                                           ,dX = nv
                                           ,dY = 0
                                           ,l = 72.0 * signum nv
                                           ,w = 24.0
                                           }
                       }

newTurtles :: Lane -> [Float] -> RiverMover
newTurtles l v = let x = case mod l 2 of 0 -> 0.0
                                         1 -> initSizeX
                     nv = v !! l
                  in Turtles {
                             aboveWater = True
                             ,ri_Entity = Entity {x = x
                                                 ,y = lanes !! l + 2
                                                 ,dX = nv
                                                 ,dY = 0
                                                 ,l = 72.0 * signum nv
                                                 ,w = 24.0
                                                 }
                             }

newLog :: Lane -> [Float] -> RiverMover
newLog l v = let x = case mod l 2 of 0 -> 0.0
                                     1 -> initSizeX
                 nv = v !! l
              in Log {
                     ri_Entity = Entity {x = x
                                         ,y = lanes !! l + 2
                                         ,dX = nv
                                         ,dY = 0
                                         ,l = 48.0 * signum nv
                                         ,w = 24.0
                                         }
                     }

newGoal :: Float -> Lane -> Goal
newGoal gx l = Goal {
                      go_Entity = Entity {x = gx
                                         ,y = lanes !! l + 2
                                         ,dX = 0.0
                                         ,dY = 0.0
                                         ,l = 24.0
                                         ,w = 24.0
                                         }
                    , is_Occupied = False
                    }

startEnv :: Int -> Env
startEnv l = let l' = (1.0 + ((fromIntegral l) / 10.0)) ^ 2
                 vels' = fmap (*l') vels
              in E { player = newPlayer
                   , goals = case l of 1         -> [newGoal ((initSizeX/2) - 12) 11]
                                       2         -> [newGoal ((initSizeX/2) - x) 11 | x <- [-22, 44]]
                                       3         -> [newGoal ((initSizeX/2) - x) 11 | x <- [-84, 12, 108]]
                                       4         -> [newGoal ((initSizeX/2) - x) 11 | x <- [-96, -24, 48, 120]]
                                       otherwise -> [newGoal ((initSizeX/2) - x) 11 | x <- [-180, -84, 12, 108, 204]]

                   , riverEnemies = concat [[setX x (newTurtles 10 vels')     | x <- xList 5]
                                           ,[setX (x-offset) (newLog 9 vels') | x <- xList 3, offset <- [0,48.0]]
                                           ,[setX x (newCroc 8 vels')         | x <- xList 9]
                                           ,[setX x (newTurtles 7 vels')      | x <- xList 6]
                                           ,[setX x (newLog 6 vels')          | x <- xList 8]
                                    ]
                   , roadEnemies = concat [[setX (x+offset) (newCar 4 vels')  | x <- xList 3
                                                                              , offset <- [0, 60.0, 120.0]]
                                          ,[setX (x+offset) (newCar 3 vels')  | x' <- xList 2
                                                                              , offset <- [0, 60.0]
                                                                              , let x = initSizeX - x']
                                          ,[setX x (newCar 2 vels')           | x <- xList 5]
                                          ,[setX (x+offset) (newCar 1 vels')  | x' <- xList 3
                                                                              , offset <- [0,50.0]
                                                                              , let x = initSizeX - x']
                                          ,[setX x (newCar 0 vels')           | x <- xList 6]
                                          ]
                   , frames = 0
                   , time = 0
                   , gameState = PreStart
                   , gameScore = 0
                   , level = 1
                   }
               where xList n = filter ((\x -> modFrac x ((screenWidth+screenEdge)/n) == 0) . (+screenEdge)) [-screenEdge+1..screenWidth]
                     modFrac n d = mod (round n) (round d)

-- CLASS INSTANCE DECLARATIONS

instance Drawable RiverMover where
  getEntity = ri_Entity
  setX x' r = let re = ri_Entity r
                  in r {ri_Entity = re {x = x'}}
  setY y' r = let re = ri_Entity r
                  in r {ri_Entity = re {y = y'}}
  setdX dx' r = let re = ri_Entity r
                    in r {ri_Entity = re {dX = dx'}}
  setdY dy' r = let re = ri_Entity r
                    in r {ri_Entity = re {dY = dy'}}
  update ri = setX (loopX $ getX ri + getdX ri) . setY (getY ri + getdY ri) $ ri
  draw c@(Croc {})
    = do color $ Color3 0.0 0.5 (0.0 :: Float)
         translate $ Vector3 (getX c) (getY c) 0.0
         scale (getL c) (getW c) 1.0
         unitSquare
         color $ Color3 1.0 1.0 (1.0 :: Float)
         preservingMatrix $ do translate $ Vector3 0.8 (0.1) (0.0 :: Float)
                               scale 0.1 0.2 (1.0 :: Float)
                               unitSquare
                               translate $ Vector3 0.0 3.0 (0.0 :: Float)
                               unitSquare
  draw t@(Turtles {})
    = do translate $ Vector3 (getX t) (getY t) 0.0
         scale (getL t / 3.0) (getW t) 1.0
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

  draw l@(Log {})
    = do color $ Color3 0.6 0.3 (0.2 :: Float)
         translate $ Vector3 (getX l) (getY l) 0.0
         scale (getL l) (getW l) 1.0
         unitSquare

instance Drawable Goal where
  getEntity = go_Entity
  setX _ = id
  setY _ = id
  setdX _ = id
  setdY _ = id
  update = id
  draw g = do if is_Occupied g then color $ Color3 0.0 1.0 (0.0 :: Float)
                               else color $ Color3 0.8 0.7 (0.2 :: Float)
              translate $ Vector3 (getX g) (getY g) 0.0
              scale (getL g) (getW g) 1.0
              unitSquare

instance Drawable Frogger where
  getEntity = fr_Entity
  setX x' f = let fe = fr_Entity f
                  in f {fr_Entity = fe {x = x'}}
  setY y' f = let fe = fr_Entity f
                  in f {fr_Entity = fe {y = y'}}
  setdX dx' f = let fe = fr_Entity f
                    in f {fr_Entity = fe {dX = dx'}}
  setdY dy' f = let fe = fr_Entity f
                    in f {fr_Entity = fe {dY = dy'}}
  draw f@(Frogger {})
    = do color $ Color3 0.0 1.0 (0.0 :: Float)
         translate $ Vector3 (getX f) (getY f) 0.0
         scale (getL f) (getW f) 1.0
         unitSquare

instance Drawable RoadMover where
  getEntity = ro_Entity
  setX x' r = let re = ro_Entity r
                  in r {ro_Entity = re {x = x'}}
  setY y' r = let re = ro_Entity r
                  in r {ro_Entity = re {y = y'}}
  setdX dx' r = let re = ro_Entity r
                    in r {ro_Entity = re {dX = dx'}}
  setdY dy' r = let re = ro_Entity r
                    in r {ro_Entity = re {dY = dy'}}
  update ro = setX (loopX $ getX ro + getdX ro) . setY (getY ro + getdY ro) $ ro
  draw c@(Car {})
    = do color $ Color3 1.0 0.0 (0.0 :: Float)
         translate $ Vector3 (getX c) (getY c) 0.0
         scale (getL c) (getW c) 1.0
         unitSquare
         color $ Color3 0.3 0.3 (1.0 :: Float)
         translate $ Vector3 0.8 0.0 (0.0 :: Float)
         scale 0.1 1.0 (1.0 :: Float)
         unitSquare

-- ADDITIONAL HELPER FUNCTIONS

loopX :: Float -> Float
loopX n = if n < (-screenEdge) then screenWidth
          else if n > screenWidth then (-screenEdge)
          else n

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

initSizeX :: Float    -- The initial width of the window
initSizeX = 640
--initSizeX = 800

initSizeY :: Float    -- The initial height of the window
initSizeY = 480.0
--initSizeY = 600

screenEdge :: Float   -- The distance off-screen for objects to be drawn
screenEdge = 100.0

screenWidth :: Float
screenWidth = initSizeX + screenEdge

lanes :: [Float]
lanes = let lane = realToFrac (initSizeY / 15) :: Float
         in tail [0.0,lane..]

vels :: [Float]
vels = [0.5, -0.3, 0.6, -0.4, 0.7, 0.0, 0.4, -0.3, 0.6, -0.8, 0.5]
