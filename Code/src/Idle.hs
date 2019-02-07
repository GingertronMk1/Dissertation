-- |Module: Frogger.Idle
module Idle where

import Graphics.UI.GLUT
import Data.IORef
import Type

-- |The IdleCallback function.
--  This is responsible for updating the game as it goes.
--  Only update the game while it is in the "Playing" state.
idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            if gameState e' == Playing then e $~! \_ -> updateEnv e'
                                       else return ()
            postRedisplay Nothing

-- |'updateEnv' is a composition of 4 functions which update the positions of moving objects, detect collision between the player and those objects, detect a collision between the player and a goal, and update the score respectively.
updateEnv :: Env -> Env
updateEnv e = let p = player e
               in if is_JumpingX p || is_JumpingY p then scoreUpdate . updateMovers $ e
                                                    else scoreUpdate . hitGoal . seeIfHit . updateMovers $ e

-- |'hitGoal' deals with the player colliding with a Goal.
--  If the goal is unoccupied, it is made occupied and the player's position is reset.
--  If the goal is occupied, the player dies.
hitGoal :: Env -> Env
hitGoal e = if gameState e == Playing
               then let (occ, nocc) = splitGoals ([],[]) . goals $ e
                     in if nocc /= []
                           then let p = player e
                                    cocc = map (\g -> (g, goalCollision p g)) occ
                                    cnocc = map (\g -> (g, goalCollision p g)) nocc
                                 in if any snd cnocc
                                       then let cGoal   = fst . head . filter snd $ cnocc
                                                gs'     = filter (/=cGoal) . goals $ e
                                                cGoal'  = cGoal {is_Occupied = True}
                                                goals'  = cGoal' : gs'
                                             in e {player = newPlayer
                                                  ,goals = goals'
                                                  ,gameScore = if all is_Occupied goals'
                                                               then gameScore e
                                                               else gameScore e + (100 * level e)
                                                  }
                                    else if any snd cocc
                                            then e {gameState = PlayerDead "You jumped on an occupied goal!"}
                                    else e
                        else e {gameState = LevelComplete}
            else e
            where splitGoals ls []        = ls
                  splitGoals (o,n) (g:gs) = if is_Occupied g then splitGoals (g:o,n) gs
                                                             else splitGoals (o,g:n) gs

-- |'scoreUpdate' is a simple function that, if the level is complete, increases the score by an amount proportional to the current level
scoreUpdate :: Env -> Env
scoreUpdate e = if gameState e == LevelComplete then e {gameScore = gameScore e + (1000 * level e)}
                                                else e

-- |'seeIfHit' calls 'hitCheck' and uses it to update either the 'GameState' of the 'Env' or the 'dX' value of the 'Frogger'
seeIfHit :: Env -> Env
seeIfHit e = let p = player e
                 (frogdX, gameState') = case hitCheck e of Left gs -> (0.0, gs)
                                                           Right v -> (v, gameState e)
              in e {player = if is_JumpingY p then p
                                              else setdX frogdX p
                   ,gameState = gameState'
                   }

-- |'updateMovers' simply applies the 'update' function required of all 'Drawable's to the all moving objects
updateMovers :: Env -> Env
updateMovers e = e {player = update . player $ e
                   ,roadEnemies = map update . roadEnemies $ e
                   ,riverEnemies = map update . riverEnemies $ e
                   }

-- |'hitCheck' first determines whether or not the 'Frogger' is in the bounds of the screen.
--  If not, the player dies.
--  If so, it checks whether or not it has collided with the roadEnemies or riverEnemies (choosing this based on the y value of the player)
--  If the player is on the road and collides with a 'RoadMover' they die.
--  If the player is on the river and does not collide with a 'RiverMover' they die, if they do collide their dX value is set to that of the collided object.
hitCheck :: Env -> Either GameState Float
hitCheck e = let frogger = player e
                 fx = getX frogger
                 fy = getY frogger
              in if inRange (0,initSizeX) fx && inRange (0,initSizeY) fy    -- If the player is in the bounds of the screen
                 then if inRange ((head lanes),(lanes !! 5)) fy                 -- If they're on the road bit
                         then Left $ hitCheckRoad frogger (roadEnemies e)
                      else if inRange ((lanes !! 6),(lanes !! 11)) fy           -- If they're on the river bit
                         then hitCheckRiver frogger (riverEnemies e)
                      else Left $ gameState e
                 else Left $ PlayerDead "You moved out of bounds!"
                 where inRange (l,u) n = case compare l u of LT -> u >= n && n >= l
                                                             EQ -> n == u
                                                             GT -> l >= n && n >= u

-- |This function came about because @elem (PlayerDead _) xs@ returns an error.
hitCheckRoad :: Frogger -> [RoadMover] -> GameState
hitCheckRoad f rs = let cs = map (roadCollision f) rs
                     in hcro' cs
                    where hcro' [] = Playing
                          hcro' (s:ss) = case s of PlayerDead _ -> s
                                                   otherwise    -> hcro' ss

-- |This function came about because @elem (PlayerDead _) xs@ returns an error.
hitCheckRiver :: Frogger -> [RiverMover] -> Either GameState Float
hitCheckRiver f rs = let cs = map (riverCollision f) rs
                      in hcri' cs
                     where hcri' [] = Left (PlayerDead "You drowned!")
                           hcri' (s:ss) = case s of Right _ -> s
                                                    Left (PlayerDead "You got eaten by a crocodile!") -> s
                                                    otherwise -> hcri' ss

-- |Detecting collision with a 'RoadMover'.
--  When the 'RoadMover' type gets more member options this will be expanded.
roadCollision :: Frogger -> RoadMover -> GameState
roadCollision f c@(Car {}) = if hasCollided f c then PlayerDead "You got hit by a car!"
                                                else Playing

-- |Detecting collision with a 'RiverMover'.
--  Stepping on a 'RiverMover' will set the Frogger's 'dX' value to that of the 'RiverMover'.
--  The exception to this is stepping on (and only on) the first third of a Croc's length - its head.
--  If you step on a Croc's head you get eaten.
riverCollision :: Frogger -> RiverMover -> Either GameState Float
riverCollision f c@(Croc {}) = let cx = getX c
                                   cy = getY c
                                   l' = (getL c)/3
                                   cw = getW c
                                   crocHead = Croc {ri_Entity = Entity {x = (2 * l') + cx
                                                                       ,y = cy
                                                                       ,l = l'
                                                                       ,w = cw
                                                                       }
                                                   }
                                   crocBody = Croc {ri_Entity = Entity {x = cx
                                                                       ,y = cy
                                                                       ,l = 2 * l'
                                                                       ,w = cw
                                                                       }
                                                   }
                                   headColl = hasCollided f crocHead
                                   bodyColl = hasCollided f crocBody
                                in if headColl && not (bodyColl) then Left $ PlayerDead "You got eaten by a crocodile!"
                                   else if not (headColl || bodyColl) then Left $ PlayerDead "You drowned!"
                                   else Right $ getdX c
riverCollision f r = if hasCollided f r then Right $ getdX r
                                        else Left $ PlayerDead "You drowned!"

-- |Detecting collision with a 'Goal'
goalCollision :: Frogger -> Goal -> Bool
goalCollision = hasCollided

-- |Detecting whether or not another 'Drawable' has collided with a 'Frogger'
hasCollided :: Drawable a => Frogger -> a -> Bool
hasCollided f d = let xf = getX f
                      yf = getY f
                      lf = getL f
                      wf = getW f
                      xd = getX d
                      yd = getY d
                      ld = getL d
                      wd = getW d
                   in case signum ld of 1         -> xd + ld > xf &&
                                                     yd + wd > yf &&
                                                     xf + lf > xd &&
                                                     yf + wf > yd
                                        -1        -> xd > xf &&
                                                     yd + wd > yf &&
                                                     xf + lf > xd + ld &&
                                                     yf + wf > yd
                                        otherwise -> False
