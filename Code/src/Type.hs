-- | Module: Frogger.Type
module Type where

import Graphics.Gloss
import System.Random

-- * Initial Values

-- | The y values for each lane, starting at the first road lane
lanes :: [Float]
lanes =
  let l = 15
      h = 3150
   in takeWhile (<= h) . map (\n -> n * h / l - 100) $ [1, 2 ..]

-- * Type Declarations

-- | 'Lane' is a type synonym for 'Int', and is shorthand for the index within 'lanes'
type Lane = Int

-- | 'Sprite' is a type denoting a Picture (the sprite itself), and a descriptor (jumping, sat, etc).
type Sprite = (String, Picture)

-- | 'SpriteID' is a type denoting a Sprite and the type of object to which it applies.
type SpriteID = (String, Sprite)

-- | The data type describing the state of the game - playing, paused, etc.
data GameState
  = -- | Before the start of the first level.
    PreStart
  | -- | While the game is in progress.
    Playing
  | -- | The player has paused the game.
    Paused
  | -- | The String here is to be used as a "cause of death" message.
    PlayerDead String
  | -- | The level is complete.
    LevelComplete
  deriving (Eq, Show)

-- | A data type to be contained by all drawn objects.
--   Cuts down on boilerplate x, y, v, l, w values all over the place.
data Entity = Entity
  { -- | Position in x
    x :: Float,
    -- | Position in y
    y :: Float,
    -- | Velocity in x
    dX :: Float,
    -- | Velocity in y
    dY :: Float,
    -- | Length
    l :: Float,
    -- | Width
    w :: Float,
    -- | The list of sprites of the Entity
    ss :: [Sprite]
  }
  deriving (Eq, Show)

-- | The data type for the player.
data Frogger = Frogger
  { -- | The Entity containing important values about the Frogger
    fr_Entity :: Entity,
    -- | A boolean flag denoting whether or not the Frogger is jumping left or right
    isJumpingX :: Bool,
    -- | A boolean flag denoting whether or not the Frogger is jumping up or down
    isJumpingY :: Bool,
    -- | If the Frogger is jumping this is its target position in x
    targetX :: Float,
    -- | If the Frogger is jumping this is its target position in y
    targetY :: Float,
    -- | The dX value the Frogger had before jumping
    prev_dX :: Float,
    -- | The dY value the Frogger had before jumping
    prev_dY :: Float,
    -- | The direction the Frogger is facing (in degrees going clockwise from North)
    facing :: Float
  }
  deriving (Eq, Show)

-- | The data type for all vehicles on the Road.
 -- | A Car
newtype RoadMover =
  Car
  {
    -- | The Entity containing important values about the Car
    ro_Entity :: Entity
  }
  deriving (Eq, Show)

-- | The data type for all objects on the River.
data RiverMover
  = -- | A Crocodile
    Croc
      { -- | The Entity containing important values about the Croc
        ri_Entity :: Entity
      }
  | -- | Some Turtles
    Turtles
      { -- | Are the Turtles above water?
        aboveWater :: Bool,
        -- | Milliseconds before the Turtles come up or submerge
        submergeTimer :: Float,
        -- | The number of seconds the Turtles should remain submerged/above water for
        surfaceDuration :: Float,
        -- | The Entity containing important values about the Turtles
        ri_Entity :: Entity
      }
  | -- | A Log
    Log
      { -- | The Entity containing important values about the Log
        ri_Entity :: Entity
      }
  deriving (Eq, Show)

-- | The data type for the goals of each level.
data Goal = Goal
  { -- | The Entity containing important values about the Goal
    go_Entity :: Entity,
    -- | Does the goal currently have a Frogger on it?
    is_Occupied :: Bool
  }
  deriving (Eq, Show)

-- | The data type that will describe the overall state, or Environment" of the game at any given time.
data Env = E
  { -- | The Frogger.
    player :: Frogger,
    -- | The enemies on the road.
    roadEnemies :: [RoadMover],
    -- | The "enemies" on the river.
    riverEnemies :: [RiverMover],
    -- | The goal/s.
    goals :: [Goal],
    -- | The total elapsed time since game start.
    time :: Float,
    -- | The current state of the game.
    gameState :: GameState,
    -- | The current score.
    gameScore :: Int,
    -- | The current level.
    level :: Int,
    -- | The width of the window in pixels.
    sWidth :: Float,
    -- | The height of the window in pixels.
    sHeight :: Float,
    -- | The random number generator used in initialisation.
    rGen :: StdGen,
    -- | The background of the game
    background :: Picture,
    -- | All sprites for the game
    spriteList :: [SpriteID]
  }
  deriving (Show)

-- * Class Declarations

-- | A typeclass which will contain all objects that are drawn to the screen.
--   This should cut down enormously on boilerplate code, and allow for some level of polymorphism whilst maintaining type clarity.
class Show a => Drawable a where
  -- | A function to draw the object to the screen.
  draw :: a -> Picture
  draw d =
    let sp = getSprites d
        spr = snd . head $ sp
        (sprL, sprW) = getSpriteSize spr
     in translate (getX d) (getY d) . scale (getL d / fromIntegral sprL) (getW d / fromIntegral sprW) $ spr

  -- | A function to draw a list of objects to the screen.
  draws :: [a] -> Picture
  draws = Pictures . map draw

  -- | Get the Entity out of the object.
  getEntity :: a -> Entity

  -- | Set the Entity of an object
  setEntity :: Entity -> a -> a

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

  -- | Get the sprites out of that entity.
  getSprites :: a -> [Sprite]
  getSprites = ss . getEntity

  -- | Northdate the x value of an entity based on its dX value.
  updateX :: a -> a
  updateX d = setX (getX d + getdX d) d

  -- | Northdate the y value of an entity based on its dY value.
  updateY :: a -> a
  updateY d = setY (getY d + getdY d) d

  -- | A function to update the object over time.
  update ::
    -- | The number of seconds to update the object by
    Float ->
    -- | The object to be updated
    a ->
    -- | The resultant object
    a
  update _ = updateX . updateY

  -- | A function to get any sprites for the Drawable
  assignSprites :: [(String, Sprite)] -> a -> a
  assignSprites sss d =
    let ider = takeWhile (/= ' ') $ show d
        sss' = map snd . filter (\x -> fst x == ider) $ sss
        en' = (getEntity d) {ss = sss'}
     in setEntity en' d

-- * Type Constructors

-- | Constructing a Frogger at the default start position.
newPlayer :: Frogger
newPlayer =
  Frogger
    { fr_Entity =
        Entity
          { x = 2000,
            y = head lanes,
            dX = 0,
            dY = 0,
            l = 180,
            w = 180,
            ss = []
          },
      isJumpingX = False,
      isJumpingY = False,
      targetX = 0,
      targetY = 0,
      prev_dX = 0,
      prev_dY = 0,
      facing = 0
    }

-- | Generating a new Car.
newCar ::
  -- | The initial x position of the Car
  Float ->
  -- | The lane the Car should occupy
  Lane ->
  -- | The list of velocities from which the Car will take its velocity
  [Float] ->
  -- | The resultant Car
  RoadMover
newCar cx cl v =
  let nv = v !! cl
   in Car
        { ro_Entity =
            Entity
              { x = cx,
                y = lanes !! cl + 10,
                dX = nv,
                dY = 0,
                l = 270 * signum nv,
                w = 180,
                ss = []
              }
        }

-- | Generating a new Croc.
newCroc ::
  -- | The initial x position of the Croc
  Float ->
  -- | The lane the Croc should occupy
  Lane ->
  -- | The list of velocities from which the Croc will take its velocity
  [Float] ->
  -- | The resultant Croc
  RiverMover
newCroc cx cl v =
  let nv = v !! cl
   in Croc
        { ri_Entity =
            Entity
              { x = cx,
                y = lanes !! cl + 10,
                dX = nv,
                dY = 0,
                l = 540 * signum nv,
                w = 180,
                ss = []
              }
        }

-- | Generating some new Turtles.
newTurtles ::
  -- | The initial x position of the Turtles
  Float ->
  -- | The lane the Turtles should occupy
  Lane ->
  -- | The list of velocities from which the Turtles will take their velocity
  [Float] ->
  -- | How long they should stay submerged/above water for (in seconds)
  Float ->
  -- | The resultant Turtles
  RiverMover
newTurtles tx tl v sd =
  let nv = v !! tl
   in Turtles
        { aboveWater = True,
          submergeTimer = 0,
          surfaceDuration = sd,
          ri_Entity =
            Entity
              { x = tx,
                y = lanes !! tl + 10,
                dX = nv,
                dY = 0,
                l = 600 * signum nv,
                w = 180,
                ss = []
              }
        }

-- | Generating a new Log.
newLog ::
  -- | The initial x position of the Log
  Float ->
  -- | The lane the Log should occupy
  Lane ->
  -- | The list of velocities from which the Log will take its velocity
  [Float] ->
  -- | The resultant Log
  RiverMover
newLog lx ll v =
  let nv = v !! ll
   in Log
        { ri_Entity =
            Entity
              { x = lx,
                y = lanes !! ll + 10,
                dX = nv,
                dY = 0,
                l = 360 * signum nv,
                w = 180,
                ss = []
              }
        }

-- | Constructing a new Goal.
newGoal ::
  -- | The x position of the Goal
  Float ->
  -- | The lane the Goal should occupy
  Lane ->
  -- | The resultant Goal
  Goal
newGoal gx gl =
  Goal
    { go_Entity =
        Entity
          { x = gx,
            y = lanes !! gl + 10,
            dX = 0,
            dY = 0,
            l = 180,
            w = 180,
            ss = []
          },
      is_Occupied = False
    }

-- | Generating the initial Env given a screen width and height.
startEnv ::
  -- | The width of the screen
  Float ->
  -- | The height of the screen
  Float ->
  -- | The random number generator
  StdGen ->
  Env
startEnv sW sH r =
  let vels = velList r
      xList n = map (+ 100) . tail $ [0, 5760 / n .. 5760]
   in E
        { player = newPlayer,
          goals = goalGen 1,
          riverEnemies =
            concat
              [ [newTurtles mx 11 vels 10 | mx <- xList 5],
                [ newLog (mx - offset) 10 vels | mx <- xList 3, offset <- [0, 500]
                ],
                [newCroc mx 9 vels | mx <- xList 5],
                [newTurtles mx 8 vels 5 | mx <- xList 6],
                [newLog mx 7 vels | mx <- xList 8]
              ],
          roadEnemies =
            concat
              [ [ newCar (mx + offset) 5 vels | mx <- xList 3, offset <- [0, 500, 1000]
                ],
                [ newCar (mx + offset) 4 vels
                  | x' <- xList 2,
                    let mx = 4000 - x',
                    offset <- [0, 500]
                ],
                [newCar mx 3 vels | mx <- xList 5],
                [ newCar (mx + offset) 2 vels
                  | x' <- xList 3,
                    let mx = 4000 - x',
                    offset <- [0, 500]
                ],
                [newCar mx 1 vels | mx <- xList 6]
              ],
          time = 0,
          gameState = PreStart,
          gameScore = 0,
          level = 1,
          sWidth = sW,
          sHeight = sH,
          rGen = r
        }

-- | The initial velocities for each lanes.
--   This is generated randomly on each call to 'startEnv'.
velList :: StdGen -> [Float]
velList = otherNeg . map (\n -> (* 3) . (+ 1) . fromIntegral $ mod n 5) . rList

-- | rList takes a StdGen and returns a randomly generated list of Ints.
rList :: StdGen -> [Int]
rList = map fst . rList'
  where
    rList' r =
      let n@(_, g) = next r
       in n : rList' g

-- | otherNeg is used to flip the velocities of every other lane.
--   It first filters the list of values less than or equal to 0, then flips the sign on each consecutive value in the list.
otherNeg :: (Ord a, Num a) => [a] -> [a]
otherNeg = otherNeg' . filter (> 0)
  where
    otherNeg' [] = []
    otherNeg' [p] = [p]
    otherNeg' (p : q : ps) =
      if p > 0
        then p : otherNeg' ((- q) : ps)
        else p : otherNeg' (q : ps)

-- * Class Instance Declarations

instance Drawable RiverMover where
  getEntity = ri_Entity
  setEntity e r = r {ri_Entity = e}
  setX x' r =
    let re = ri_Entity r
     in r {ri_Entity = re {x = x'}}
  setY y' r =
    let re = ri_Entity r
     in r {ri_Entity = re {y = y'}}
  setdX dx' r =
    let re = ri_Entity r
     in r {ri_Entity = re {dX = dx'}}
  setdY dy' r =
    let re = ri_Entity r
     in r {ri_Entity = re {dY = dy'}}
  update ms t@Turtles {aboveWater = aw, submergeTimer = st, surfaceDuration = sd} =
    let sd' = if aw then sd else sd / 2 -- They're underwater for half the time they're above it for
        st' =
          let new = st + ms -- New timer = old plus ∂t
           in if new >= sd' then 0 else new -- Unless that == duration in which case its 0
        aw' =
          if st' == 0
            then not aw -- If it is 0 then surface/submerge
            else aw -- Otherwise don't
     in setX (loopX $ getX t + getdX t) . setY (getY t + getdY t) $
          t
            { aboveWater = aw',
              submergeTimer = st'
            }
  update _ ri = setX (loopX $ getX ri + getdX ri) . setY (getY ri + getdY ri) $ ri
  draw t@Turtles {aboveWater = aw} =
    let spr =
          if aw
            then snd . head . filter ((== "surfaced") . fst) . getSprites $ t
            else snd . head . filter ((== "submerged") . fst) . getSprites $ t
        (sprL, sprW) = getSpriteSize spr
     in translate (getX t) (getY t) . scale (negate (getL t / fromIntegral sprL)) (getW t / fromIntegral sprW) $ spr
  draw d =
    let sp = getSprites d
        spr = snd . head $ sp
        (sprL, sprW) = getSpriteSize spr
     in translate (getX d) (getY d) . scale (getL d / fromIntegral sprL) (getW d / fromIntegral sprW) $ spr

instance Drawable Goal where
  getEntity = go_Entity
  setEntity e g = g {go_Entity = e}
  setX _ = id
  setY _ = id
  setdX _ = id
  setdY _ = id
  update _ = id
  draw g =
    let sp = getSprites g
        spr' =
          if is_Occupied g
            then filter ((== "occupied") . fst) sp
            else filter ((/= "occupied") . fst) sp
        spr = snd . head $ spr'
        (sprL, sprW) = getSpriteSize spr
     in translate (getX g) (getY g) . scale (getL g / fromIntegral sprL) (getW g / fromIntegral sprW) $ spr

instance Drawable Frogger where
  getEntity = fr_Entity
  setEntity e f = f {fr_Entity = e}
  setX x' f =
    let fe = fr_Entity f
     in f {fr_Entity = fe {x = x'}}
  setY y' f =
    let fe = fr_Entity f
     in f {fr_Entity = fe {y = y'}}
  setdX dx' f =
    let fe = fr_Entity f
     in f {fr_Entity = fe {dX = dx'}}
  setdY dy' f =
    let fe = fr_Entity f
     in f {fr_Entity = fe {dY = dy'}}
  update _ f@Frogger {isJumpingX = ijx, isJumpingY = ijy, targetX = tx, targetY = ty, prev_dX = pdx, prev_dY = pdy}
    | ijx && getX f == tx = updateResetdXdY $ f {isJumpingX = False}
    | ijy && getY f == ty = updateResetdXdY $ f {isJumpingY = False}
    | otherwise = updateXY f
    where
      updateXY = updateX . updateY
      updateResetdXdY = updateXY . setdX pdx . setdY pdy
  draw f@Frogger {facing = dir} =
    case getSprites f of
      [] ->
        let darkGreen = makeColor 0.2 0.8 0.2 1.0
            xf = getX f
            yf = getY f
            lf = getL f
            wf = getW f
            (x', y', l', w') = if isJumping f then (xf, yf, lf * 1.1, wf * 1.1) else (xf, yf, lf, wf)
         in translate x' y' . scale l' w' $
              Pictures $
                map
                  (rotate dir)
                  [ color darkGreen $ rectangleSolid 1.0 1.0,
                    color white . translate 0.4 0.4 $ rectangleSolid 0.1 0.1,
                    color white . translate (-0.4) 0.4 $ rectangleSolid 0.1 0.1
                  ]
      _ ->
        let spr =
              if isJumping f
                then snd . head . filter ((== "jumping") . fst) . getSprites $ f
                else snd . head . filter ((== "landed") . fst) . getSprites $ f
            (sprL, sprW) = getSpriteSize spr
         in translate (getX f) (getY f)
              . scale (getL f / fromIntegral sprL) (getW f / fromIntegral sprW)
              . rotate dir
              $ spr

instance Drawable RoadMover where
  getEntity = ro_Entity
  setEntity e r = r {ro_Entity = e}
  setX x' r =
    let re = ro_Entity r
     in r {ro_Entity = re {x = x'}}
  setY y' r =
    let re = ro_Entity r
     in r {ro_Entity = re {y = y'}}
  setdX dx' r =
    let re = ro_Entity r
     in r {ro_Entity = re {dX = dx'}}
  setdY dy' r =
    let re = ro_Entity r
     in r {ro_Entity = re {dY = dy'}}
  update _ ro = setX (loopX $ getX ro + getdX ro) . setY (getY ro + getdY ro) $ ro

-- * Additional Helper Functions

-- | 'assignAllSprites' takes the sprite list from an Env and assigns sprites to all Drawables within it.
assignAllSprites :: Env -> Env
assignAllSprites e@E {player = p, roadEnemies = roE, riverEnemies = riE, goals = g, spriteList = sprs} =
  e
    { player = assignSprites sprs p,
      roadEnemies = map (assignSprites sprs) roE,
      riverEnemies = map (assignSprites sprs) riE,
      goals = map (assignSprites sprs) g
    }

-- | 'getSpriteSize' returns a tuple containing the Sprites length and width.
getSpriteSize :: Picture -> (Int, Int)
getSpriteSize (BitmapSection Rectangle {rectPos = _, rectSize = lw} _) = lw
getSpriteSize (Pictures xs) = getMaxSize (0, 0) $ map getSpriteSize xs
  where
    getMaxSize lw [] = lw
    getMaxSize (l', w') ((l, w) : lws) =
      if l' < l && w' < w
        then getMaxSize (l, w) lws
        else getMaxSize (l', w') lws
getSpriteSize _ = (0, 0)

-- | 'isJumping' does as the name suggests, returning a Bool with whether or not the Frogger is jumping.
isJumping :: Frogger -> Bool
isJumping f = isJumpingX f || isJumpingY f

-- | A function to split a Croc into its head and body - used in collision detection.
splitCroc ::
  -- | The intial Croc
  RiverMover ->
  -- | Its head and body
  (RiverMover, RiverMover)
splitCroc c@Croc {} =
  let cx = getX c
      cy = getY c
      l' = getL c / 3
      cw = getW c
      crocHead =
        Croc
          { ri_Entity =
              Entity
                { x = cx + l' / 2,
                  y = cy,
                  l = l',
                  w = cw,
                  dX = 0,
                  dY = 0,
                  ss = []
                }
          }
      crocBody =
        Croc
          { ri_Entity =
              Entity
                { x = cx - l',
                  y = cy,
                  l = 2 * l',
                  w = cw,
                  dX = 0,
                  dY = 0,
                  ss = []
                }
          }
   in (crocHead, crocBody)
splitCroc rm = (rm, rm)

-- | 'loopX' is used to loop the x-value of moving objects.
loopX :: Float -> Float
loopX n
  | n < leftMost = n + 5760
  | n > rightMost = n - 5760
  | otherwise = n
  where
    leftMost = -880
    rightMost = 4880

-- | Generating goals for a given level.
goalGen ::
  -- | The level for which to generate the Goals
  Int ->
  [Goal]
goalGen lev
  | lev == 1 = newGoals [0]
  | lev == 2 = newGoals [-200, 200]
  | lev == 3 = newGoals [-400, 0, 400]
  | lev == 4 = newGoals [-600, -200, 200, 600]
  | otherwise = newGoals [-800, -400, 0, 400, 800]
  where
    newGoals = map (\o -> newGoal (2000 + o) 12)
