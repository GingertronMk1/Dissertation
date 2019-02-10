-- |Module: Frogger.Type
module Type where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap

-- * Initial Values

-- | The y values for each lane, starting at the first road lane
lanes :: [Float]
lanes = [200,400..3000]

-- | The initial velocities for each lanes
--   Ideally this will at some point be generated more randomly, however for the moment it is statically defined
vels :: [Float]
vels = [3.0, -1.8, 3.6, -2.4, 4.2, 0.0, 2.4, -1.8, 3.6, -4.8, 3.0]

-- * Type Declarations

-- | 'Lane' is a type synonym for 'Int', and is shorthand for the index within 'lanes'
type Lane = Int

-- | A data type to be contained by all drawn objects
--   Cuts down on boilerplate x, y, v, l, w values all over the place
data Entity = Entity { x :: Float   -- ^ Position in x
                     , y :: Float   -- ^ Position in y
                     , dX :: Float  -- ^ Velocity in x
                     , dY :: Float  -- ^ Velocity in y
                     , l  :: Float  -- ^ Length
                     , w  :: Float  -- ^ Width
              }
              deriving (Eq, Show)

-- | The data type for the player.
data Frogger = Frogger { fr_Entity   :: Entity -- ^ The Entity containing important values about the Frogger
                       , is_JumpingX :: Bool   -- ^ A boolean flag denoting whether or not the Frogger is jumping left or right
                       , is_JumpingY :: Bool   -- ^ A boolean flag denoting whether or not the Frogger is jumping up or down
                       , targetX     :: Float  -- ^ If the Frogger is jumping this is its target position in x
                       , targetY     :: Float  -- ^ If the Frogger is jumping this is its target position in y
                       , prev_dX     :: Float  -- ^ The dX value the Frogger had before jumping
                       , prev_dY     :: Float  -- ^ The dY value the Frogger had before jumping
               }
               deriving (Eq, Show)

-- | The data type for all vehicles on the Road.
data RoadMover = -- | A Car
                 Car { ro_Entity :: Entity -- ^ The Entity containing important values about the Car
                 }
                 deriving (Eq, Show)

-- | The data type for all objects on the River.
data RiverMover = -- | A Crocodile
                  Croc { ri_Entity :: Entity    -- ^ The Entity containing important values about the Croc
                  }
                  -- | Some Turtles
                | Turtles { aboveWater :: Bool          -- ^ Are the Turtles above water?
                          , submergeTimer :: Float      -- ^ Milliseconds before the Turtles come up or submerge
                          , submergeDuration  :: Float  -- ^ The number of seconds the Turtles should remain submerged/above water for
                          , ri_Entity :: Entity         -- ^ The Entity containing important values about the Turtles
                }
                  -- | A Log
                | Log { ri_Entity :: Entity     -- ^ The Entity containing important values about the Log
                }
                deriving (Eq, Show)

-- | The data type for the goals of each level.
data Goal =  Goal { go_Entity :: Entity -- ^ The Entity containing important values about the Goal
                  , is_Occupied :: Bool -- ^ Does the goal currently have a Frogger on it?
          }
          deriving (Eq, Show)

-- | The data type describing the state of the game - playing, paused, etc.
data GameState = PreStart           -- ^ Before the start of the first level.
               | Playing            -- ^ While the game is in progress.
               | Paused             -- ^ The player has paused the game.
               | PlayerDead String  -- ^ The String here is to be used as a "cause of death" message.
               | LevelComplete      -- ^ The level is complete.
               deriving (Eq, Show)


-- | The data type that will describe the overall state, or Environment" of the game at any given time.
data Env = E { player       :: Frogger      -- ^The Frogger.
             , roadEnemies  :: [RoadMover]  -- ^The enemies on the road.
             , riverEnemies :: [RiverMover] -- ^The "enemies" on the river.
             , goals        :: [Goal]       -- ^The goal/s.
             , frames       :: Int          -- ^The total elapsed number of frames.
             , time         :: Float        -- ^The total elapsed time since game start.
             , gameState    :: GameState    -- ^The current state of the game.
             , gameScore    :: Int          -- ^The current score.
             , level        :: Int          -- ^The current level.
             , sWidth  :: Float             -- ^The width of the window in pixels.
             , sHeight :: Float             -- ^The height of the window in pixels.
         }
         deriving (Eq, Show)

-- * Class Declarations

-- | A typeclass which will contain all objects that are drawn to the screen.
--   This should cut down enormously on boilerplate code, and allow for some level of polymorphism whilst maintaining type clarity.
class Drawable a where
  -- | A function to draw the object to the screen.
  draw :: a -> Picture
  -- | A function to draw a list of objects to the screen.
  draws :: [a] -> Picture
  draws = Pictures . map draw
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
  update :: Float -- ^ The number of seconds to update the object by
         -> a     -- ^ The object to be updated
         -> a     -- ^ The resultant object
  update t = updateX . updateY

-- * Type Constructors
  
-- | Constructing a Frogger at the default start position
newPlayer :: Frogger
newPlayer = Frogger {fr_Entity = Entity {x  = 1910
                                        ,y  = 10
                                        ,dX = 0
                                        ,dY = 0
                                        ,l  = 180
                                        ,w  = 180
                                        }
                     ,is_JumpingX = False
                     ,is_JumpingY = False
                     ,targetX = 0
                     ,targetY = 0
                     ,prev_dX = 0
                     ,prev_dY = 0
                     }

-- | Generating a new Car
newCar :: Float     -- ^ The initial x position of the Car
       -> Lane      -- ^ The lane the Car should occupy
       -> [Float]   -- ^ The list of velocities from which the Car will take its velocity
       -> RoadMover -- ^ The resultant Car
newCar cx l v = let nv = v !! l
                 in Car {ro_Entity = Entity {x = cx
                                            ,y = lanes !! l + 10
                                            ,dX = nv
                                            ,dY = 0
                                            ,l = 360 * signum nv
                                            ,w = 180
                                            }
                        }

-- | Generating a new Croc
newCroc :: Float      -- ^ The initial x position of the Croc
        -> Lane       -- ^ The lane the Croc should occupy
        -> [Float]    -- ^ The list of velocities from which the Croc will take its velocity
        -> RiverMover -- ^ The resultant Croc
newCroc cx l v = let nv = v !! l
                  in Croc {ri_Entity = Entity {x = cx
                                              ,y = lanes !! l + 10
                                              ,dX = nv
                                              ,dY = 0
                                              ,l = 540 * signum nv
                                              ,w = 180
                                              }
                          }

-- | Generating some new Turtles
newTurtles :: Float       -- ^ The initial x position of the Turtles
           -> Lane        -- ^ The lane the Turtles should occupy
           -> [Float]     -- ^ The list of velocities from which the Turtles will take their velocity
           -> Float       -- ^ How long they should stay submerged/above water for (in seconds)
           -> RiverMover  -- ^ The resultant Turtles
newTurtles tx l v sd = let nv = v !! l
                        in Turtles {aboveWater = True
                                   ,submergeTimer = 0
                                   ,submergeDuration = sd
                                   ,ri_Entity = Entity {x = tx
                                                       ,y = lanes !! l + 10
                                                       ,dX = nv
                                                       ,dY = 0
                                                       ,l = 600 * signum nv
                                                       ,w = 180
                                                       }
                                   }

-- | Generating a new Log
newLog :: Float       -- ^ The initial x position of the Log
       -> Lane        -- ^ The lane the Log should occupy
       -> [Float]     -- ^ The list of velocities from which the Log will take its velocity
       -> RiverMover  -- ^ The resultant Log
newLog lx l v = let nv = v !! l
                 in Log {ri_Entity = Entity {x = lx
                                            ,y = lanes !! l + 10
                                            ,dX = nv
                                            ,dY = 0
                                            ,l = 360 * signum nv
                                            ,w = 180
                                            }
                        }

-- | Constructing a new Goal
newGoal :: Float  -- ^ The x position of the Goal
        -> Lane   -- ^ The lane the Goal should occupy
        -> Goal   -- ^ The resultant Goal
newGoal gx l = Goal {go_Entity = Entity {x = gx
                                        ,y = lanes !! l + 10
                                        ,dX = 0
                                        ,dY = 0
                                        ,l = 180
                                        ,w = 180
                                        }
                    ,is_Occupied = False
                    }

-- | Generating the initial Env
startEnv :: Env
startEnv = E {player = newPlayer
             ,goals = goalGen 1
             ,riverEnemies = concat [[newTurtles x 10 vels 6    | x <- xList 5]
                                    ,[newLog (x-offset) 9 vels  | x <- xList 3
                                                                , offset <- [0,500]]
                                    ,[newCroc x 8 vels          | x <- xList 9]
                                    ,[newTurtles x 7 vels 5     | x <- xList 6]
                                    ,[newLog x 6 vels           | x <- xList 8]
                                    ]
             ,roadEnemies = concat [[newCar (x+offset) 4 vels   | x <- xList 3
                                                                , offset <- [0, 500, 1000]]
                                   ,[newCar (x+offset) 3 vels   | x' <- xList 2
                                                                , offset <- [0, 500]
                                                                , let x = 4000 - x']
                                   ,[newCar x 2 vels            | x <- xList 5]
                                   ,[newCar (x+offset) 1 vels   | x' <- xList 3
                                                                , offset <- [0,500]
                                                                , let x = 4000 - x']
                                   ,[newCar x 0 vels            | x <- xList 6]
                                   ]
             ,frames = 0
             ,time = 0
             ,gameState = PreStart
             ,gameScore = 0
             ,level = 1
             ,sWidth = 1    -- This will be updated immediately
             ,sHeight = 1   -- This will be updated immediately
             }
             where xList n = tail [0,5760/n..5760]

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
  update ms t@Turtles {aboveWater = aw, submergeTimer = st, submergeDuration = sd}
    = let st' = let new = st + ms
                 in if new >= sd then 0 else new
          aw' = if st' == 0 then not aw
                            else aw
       in setX (loopX $ getX t + getdX t) . setY (getY t + getdY t) $ t {aboveWater = aw', submergeTimer = st'}
  update ms ri = setX (loopX $ getX ri + getdX ri) . setY (getY ri + getdY ri) $ ri
  draw c@Croc {} = let (cHead, cBody) = splitCroc c
                       bodyGreen = makeColor 0.0 1.0 0.0 1.0
                       headGreen = makeColor 0.0 0.8 0.0 1.0
                    in Pictures [Color bodyGreen . translate (getX cBody) (getY cBody) . scale (getL cBody) (getW cBody) $ unitSquare
                                ,Color headGreen . translate (getX cHead) (getY cHead) . scale (getL cHead) (getW cHead) $ unitSquare]
  draw t@Turtles {aboveWater = aw} = let l' = getL t / 3
                                         xt = getX t
                                         yt = getY t
                                         wt = getW t
                                         mask = if not aw then [Color alphaBlue . translate xt yt . scale (3*l') wt $ unitSquare]
                                                          else []
                                         alphaBlue = makeColor 0.0 0.0 1.0 0.2
                                         drawTurtle x = Pictures [
                                                                  Color green  . translate (x+(0.1*l')) (yt+(0.1*wt)). scale (0.8*l') (0.8*wt) $ unitSquare
                                                                 ,Color orange . translate x yt . scale l' wt $ unitCircle
                                                                 ]
                                      in Pictures $ [drawTurtle (xt + (l' * off))| off <- [0,1,2]] ++ mask
  draw l@Log {} = let brown = makeColor 0.6 0.3 0.1 1.0
                   in Color brown . translate (getX l) (getY l) . scale (getL l) (getW l) $ unitSquare

instance Drawable Goal where
  getEntity = go_Entity
  setX _ = id
  setY _ = id
  setdX _ = id
  setdY _ = id
  update _ = id
  draw g = let gx = getX g
               gy = getY g
               dg = Color yellow . translate (getX g) (getY g) . scale (getL g) (getW g) $ unitSquare
            in if is_Occupied g then Pictures [dg, draw . setX gx . setY gy $ newPlayer]
                                else dg

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
  update ms f@Frogger{is_JumpingX = ijx, is_JumpingY = ijy, targetX = tx, targetY = ty, prev_dX = pdx, prev_dY = pdy}
    | ijx && getX f == tx = updateResetdXdY $ f {is_JumpingX = False}
    | ijy && getY f == ty = updateResetdXdY $ f {is_JumpingY = False}
    | otherwise           = updateXY f
    where updateXY = updateX . updateY
          updateResetdXdY = updateXY . setdX pdx . setdY pdy
  draw f@Frogger {} = let darkGreen = makeColor 0.2 0.8 0.2 1.0
                          xf = getX f
                          yf = getY f
                          lf = getL f
                          wf = getW f
                          (x',y',l',w') = if is_Jumping f then (xf - 0.05 * w', yf - 0.05 * l', lf * 1.1, wf * 1.1) else (xf,yf,lf,wf)
                       in Color darkGreen . translate x' y' . scale l' w' $ unitSquare

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
  update ms ro = setX (loopX $ getX ro + getdX ro) . setY (getY ro + getdY ro) $ ro
  draw c@(Car {}) = Color red . translate (getX c) (getY c) . scale (getL c) (getW c) $ unitSquare

-- * Additional Helper Functions

-- | 'is_Jumping' does as the name suggests, returning a Bool with whether or not the Frogger is jumping
is_Jumping :: Frogger -> Bool
is_Jumping f = is_JumpingX f || is_JumpingY f

-- | A function to split a Croc into its head and body - used in collision detection and drawing
splitCroc :: RiverMover               -- ^ The intial Croc
          -> (RiverMover, RiverMover) -- ^ Its head and body
splitCroc c@Croc {} = let cx = getX c
                          cy = getY c
                          l' = (getL c)/3
                          cw = getW c
                          crocHead = Croc {ri_Entity = Entity {x = (2 * l') + cx
                                                              ,y = cy
                                                              ,l = l'
                                                              ,w = cw
                                                              ,dX = 0
                                                              ,dY = 0
                                                              }
                                          }
                          crocBody = Croc {ri_Entity = Entity {x = cx
                                                              ,y = cy
                                                              ,l = 2 * l'
                                                              ,w = cw
                                                              ,dX = 0
                                                              ,dY = 0
                                                              }
                                          }
                       in (crocHead, crocBody)

-- |'loopX' is used to loop the x-value of moving objects
loopX :: Float -> Float
loopX n
  | n < leftMost  = n + 5760
  | n > rightMost = n - 5760
  | otherwise     = n
  where leftMost  = -880
        rightMost = 4880

-- | Generating goals for a given level
goalGen :: Int    -- ^ The level for which to generate the Goals
        -> [Goal]
goalGen l
  | l == 1    = newGoals [0]
  | l == 2    = newGoals [-200,200]
  | l == 3    = newGoals [-400,0,400]
  | l == 4    = newGoals [-600,-200,200,600]
  | otherwise = newGoals [-800,-400,0,400,800]
  where newGoals = map (\x -> newGoal (1910+x) 11)

-- | Drawing a square of size 1x1
unitSquare :: Picture
unitSquare = translate 0.5 0.5 $ rectangleSolid 1.0 1.0

-- | Drawing a circle of diameter 1
unitCircle :: Picture
unitCircle = translate 0.5 0.5 $ circleSolid 0.5
