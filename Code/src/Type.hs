-- |Module: Frogger.Type
module Type where

import Graphics.UI.GLUT

-- * Initial Values

-- |The initial width of the window.
--  The window is initially set to 640x480px.
initSizeX :: Float
initSizeX = 640

-- |The initial height of the window.
--  The window is initially set to 640x480px.
initSizeY :: Float
initSizeY = 480.0

-- |The distance off-screen for objects to be drawn.
-- This is to be used when an object "loops" around, such that it does not disappear as soon as it reaches the edge of the screen.
screenEdge :: Float
screenEdge = 100.0

-- |The full distance from the origin to the edge of the "screen" in x.
screenWidth :: Float
screenWidth = initSizeX + screenEdge

-- |The y values for each lane, starting at the first road lane
lanes :: [Float]
lanes = let lane = realToFrac (initSizeY / 15) :: Float
         in tail [0.0,lane..]

-- |The initial velocities for each lanes
--  Ideally this will at some point be generated more randomly, however for the moment it is statically defined
vels :: [Float]
vels = [0.5, -0.3, 0.6, -0.4, 0.7, 0.0, 0.4, -0.3, 0.6, -0.8, 0.5]

-- * Type Declarations

-- |'Lane' is a type synonym for 'Int', and is shorthand for the index within 'lanes'
type Lane = Int

-- |A data type to be contained by all drawn objects
--  Cuts down on boilerplate x, y, v, l, w values all over the place
data Entity = Entity {
                       x :: Float   -- ^ Position in x
                     , y :: Float   -- ^ Position in y
                     , dX :: Float  -- ^ Velocity in x
                     , dY :: Float  -- ^ Velocity in y
                     , l  :: Float  -- ^ Length
                     , w  :: Float  -- ^ Width
              }
              deriving (Eq, Show)

-- |The data type for the player.
data Frogger = Frogger {fr_Entity   :: Entity -- ^ The Entity containing important values about the Frogger
                       ,is_JumpingX :: Bool   -- ^ A boolean flag denoting whether or not the Frogger is jumping left or right
                       ,is_JumpingY :: Bool   -- ^ A boolean flag denoting whether or not the Frogger is jumping up or down
                       ,targetX     :: Float  -- ^ If the Frogger 'is_Jumping' this is its target position in x
                       ,targetY     :: Float  -- ^ If the Frogger 'is_Jumping' this is its target position in y
                       ,prev_dX     :: Float  -- ^ The dX value the Frogger had before jumping
                       ,prev_dY     :: Float  -- ^ The dY value the Frogger had before jumping
               }
               deriving (Eq, Show)

-- |The data type for all vehicles on the Road.
data RoadMover = -- | A Car
                 Car {ro_Entity :: Entity -- ^ The Entity containing important values about the Car
                 }
                 deriving (Eq, Show)

-- |The data type for all objects on the River.
data RiverMover = -- | A Crocodile
                  Croc { ri_Entity :: Entity    -- ^ The Entity containing important values about the Croc
                  }
                  -- | Some Turtles
                | Turtles { aboveWater :: Bool  -- ^ Are the turtles above water?
                          , ri_Entity :: Entity -- ^ The Entity containing important values about the Turtles
                }
                  -- | A Log
                | Log { ri_Entity :: Entity     -- ^ The Entity containing important values about the Log
                }
                deriving (Eq, Show)

-- |The data type for the goals of each level.
data Goal =  Goal { go_Entity :: Entity -- ^ The Entity containing important values about the Goal
                  , is_Occupied :: Bool -- ^ Does the goal currently have a Frogger on it?
          }
          deriving (Eq, Show)

-- |A data type describing the state of the game - playing, paused, etc.
data GameState = PreStart           -- ^ Before the start of the first level.
               | Playing            -- ^ While the game is in progress.
               | Paused             -- ^ The player has paused the game.
               | PlayerDead String  -- ^ The String here is to be used as a "cause of death" message.
               | LevelComplete      -- ^ The level is complete.
               deriving (Eq, Show)


-- |The data type that will describe the overall state, or Environment" of the game at any given time.
data Env = E { player :: Frogger            -- ^The Frogger.
             , roadEnemies :: [RoadMover]   -- ^The enemies on the road.
             , riverEnemies :: [RiverMover] -- ^The "enemies" on the river.
             , goals :: [Goal]              -- ^The goal/s.
             , frames :: Int                -- ^The total elapsed number of frames.
             , time :: Float                -- ^The total elapsed time since game start.
             , gameState :: GameState       -- ^The current state of the game.
             , gameScore :: Int             -- ^The current score.
             , level :: Int                 -- ^The current level.
         }
         deriving (Eq, Show)

-- * Class Declarations

-- |A typeclass which will contain all objects that are drawn to the screen.
--  This should cut down enormously on boilerplate code, and allow for some level of polymorphism whilst maintaining type clarity.
class Drawable a where
  -- | A function to draw the object to the screen.
  draw :: a -> IO()
  -- | Get the Entity out of the object.
  getEntity :: a -> Entity
  -- | Set the x value of the entity within the Drawable.
  setX :: Float -> a -> a
  -- | Set the y value of the entity within the Drawable.
  setY :: Float -> a -> a
  -- | Set the dX value of the entity within the Drawable.
  setdX :: Float -> a -> a
  -- | Set the dY value of the entity within the Drawable.
  setdY :: Float -> a -> a
  -- | A function to draw the object, preserving the current transformation matrix.
  preservingDraw :: a -> IO()
  preservingDraw = preservingMatrix . draw
  -- | Preserved drawing multiple items in a list.
  preservingDraws :: [a] -> IO[()]
  preservingDraws = sequence . map preservingDraw
  -- | Get the x value out of that entity.
  getX :: a -> Float
  getX = x . getEntity
  -- | Get the y value out of that entity.
  getY :: a -> Float
  getY = y . getEntity
  -- | Get the dX value out of that entity.
  getdX :: a -> Float
  getdX = dX . getEntity
  -- | Get the dY value out of that entity.
  getdY :: a -> Float
  getdY = dY . getEntity
  -- | Get the length value out of that entity.
  getL :: a -> Float
  getL = l . getEntity
  -- | Get the width value out of that entity.
  getW :: a -> Float
  getW = w . getEntity
  -- | Update the x value of an entity based on its dX value.
  updateX :: a -> a
  updateX d = setX (getX d + getdX d) d
  -- | Update the y value of an entity based on its dY value.
  updateY :: a -> a
  updateY d = setY (getY d + getdY d) d
  -- | A function to update the object over time.
  update :: a -> a
  update = updateX . updateY

-- * Type Constructors

newPlayer :: Frogger  -- ^ Constructing a Frogger at the default start position
newPlayer = setY 4.0 . (\s -> setX ((initSizeX - getW s)/2) s) $ Frogger {fr_Entity = Entity {dX = 0
                                                                                             ,dY = 0
                                                                                             ,l  = 20
                                                                                             ,w  = 20
                                                                                             }
                                                                         ,is_JumpingX = False
                                                                         ,is_JumpingY = False
                                                                         ,targetX = 0.0
                                                                         ,targetY = 0.0
                                                                         ,prev_dX = 0.0
                                                                         ,prev_dY = 0.0
                                                                         }

newCar :: Lane      -- ^ The lane the Car should occupy
       -> [Float]   -- ^ The list of velocities from which the car will take its
       -> RoadMover -- ^ The resultant Car
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

newCroc :: Lane       -- ^ The lane the Croc should occupy
        -> [Float]    -- ^ The list of velocities from which the car will take its
        -> RiverMover -- ^ The resultant Croc
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

newTurtles :: Lane        -- ^ The lane the Turtles should occupy
           -> [Float]     -- ^ The list of velocities from which the car will take its
           -> RiverMover  -- ^ The resultant Turtles
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

newLog :: Lane        -- ^ The lane the Log should occupy
       -> [Float]     -- ^ The list of velocities from which the car will take its
       -> RiverMover  -- ^ The resultant Log
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

-- |Constructing a new Goal in lane l and at x position gx
newGoal :: Float  -- ^ The x position of the Goal
        -> Lane   -- ^ The lane the Goal should occupy
        -> Goal   -- ^ The resultant Goal
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

startEnv :: Int   -- ^ The level of the new Env
         -> Env
startEnv l = let l' = (1.0 + ((fromIntegral l) / 10.0)) ^ 2
                 vels' = fmap (*l') vels
              in E { player = newPlayer
                   , goals = case l of
                                       1         -> [newGoal ((initSizeX/2) - 12) 11]

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
               where -- |The function to generate the list of x values for a given lane, of length n
                     xList n = filter ((\x -> modFrac x ((screenWidth+screenEdge)/n) == 0) . (+screenEdge)) [-screenEdge+1..screenWidth]
                     modFrac n d = mod (round n) (round d)

-- * Class Instance Declarations

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
  update f@(Frogger{fr_Entity = fe, is_JumpingX = ijx, is_JumpingY = ijy, targetX = tx, targetY = ty, prev_dX = pdx, prev_dY = pdy})
    =      if ijy && getY f == ty then updateX . updateY . setdX pdx . setdY pdy $ f {is_JumpingY = False}
      else if ijx && getX f == tx then updateX . updateY . setdX pdx . setdY pdy $ f {is_JumpingX = False}
      else                             updateX . updateY $ f
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

-- * Additional Helper Functions

-- |'loopX' is used to loop the x-value of moving objects
loopX :: Float -> Float
loopX n = if n < (-screenEdge) then screenWidth
          else if n > screenWidth then (-screenEdge)
          else n

-- |Drawing a circle of radius 1
unitCircle :: IO()
unitCircle = let n = 50.0
                 points = [(0.5*(sin (2*pi*k/n)+1.0), 0.5*(cos (2*pi*k/n)+1.0), 0) | k <- [1..n]]
              in (renderPrimitive Polygon . mapM_ makeVertex) points

-- |Drawing a square of size 1x1
unitSquare :: IO()
unitSquare = let us = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float, Float, Float)]
             in (renderPrimitive Quads . mapM_ makeVertex) us

-- |A function to essentially curry the vertex constructor in the GLUT library
makeVertex :: (Float, Float, Float) -> IO()
makeVertex (x,y,z) = vertex $ Vertex3 x y z
