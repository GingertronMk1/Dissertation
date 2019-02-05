module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            if gameState e' == Playing then e $~! \_ -> updateEnv e'
                                       else return ()
            postRedisplay Nothing

updateEnv :: Env -> Env
updateEnv = scoreUpdate . hitGoal . seeIfHit . updateMovers

hitGoal :: Env -> Env
hitGoal e = if gameState e == Playing
               then let (occ, nocc) = splitGoals ([],[]) . goals $ e                           -- Split the list of goals into those that are occupied and those that aren't
                     in if nocc /= [] 
                           then let p = player e                                    -- Pull the player out because we're gonna use it a bit
                                    cocc = map (\g -> (g, goalCollision p g)) occ   -- :: [(Goal, Bool] where the goal is occupied and the Bool is whether or not that Goal has been hit
                                    cnocc = map (\g -> (g, goalCollision p g)) nocc -- :: [(Goal, Bool] where the goal is not occupied and the Bool is whether or not that Goal has been hit
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

splitGoals :: ([Goal],[Goal]) -> [Goal] -> ([Goal],[Goal])
splitGoals ls []        = ls
splitGoals (o,n) (g:gs) = if is_Occupied g then splitGoals (g:o,n) gs
                                           else splitGoals (o,g:n) gs

scoreUpdate :: Env -> Env
scoreUpdate e = if gameState e == LevelComplete then e {gameScore = gameScore e + (1000 * level e)}
                                                else e

seeIfHit :: Env -> Env
seeIfHit e = let (frogdX, gameState') = case hitCheck e of Left gs -> (0.0, gs)
                                                           Right v -> (v, gameState e)
              in e {player = setdX frogdX . player $ e
                   ,gameState = gameState'
                   }

updateMovers :: Env -> Env
updateMovers e = e {player = update . player $ e
                   ,roadEnemies = map update . roadEnemies $ e
                   ,riverEnemies = map update . riverEnemies $ e
                   }

hitCheck :: Env -> Either GameState Float
hitCheck e = let frogger = player e
                 fx = getX frogger
                 fy = getY frogger
              in if inRange (0,initSizeX) fx && inRange (0,initSizeY) fy    -- If the player is in the bounds of the screen
                 then if inRange ((head lanes),(lanes !! 4)) fy                 -- If they're on the road bit
                         then Left $ hitCheckRoad frogger (roadEnemies e)
                      else if inRange ((lanes !! 6),(lanes !! 11)) fy           -- If they're on the river bit
                         then hitCheckRiver frogger (riverEnemies e)
                      else Left $ gameState e
                 else Left $ PlayerDead "You moved out of bounds!"
                 where inRange (l,u) n = case compare l u of LT -> u >= n && n >= l
                                                             EQ -> n == u
                                                             GT -> l >= n && n >= u

hitCheckRoad :: Frogger -> [RoadMover] -> GameState
hitCheckRoad f rs = let cs = map (roadCollision f) rs
                     in hcro' cs
                    where hcro' [] = Playing
                          hcro' (s:ss) = case s of PlayerDead _ -> s
                                                   otherwise    -> hcro' ss

hitCheckRiver :: Frogger -> [RiverMover] -> Either GameState Float
hitCheckRiver f rs = let cs = map (riverCollision f) rs
                      in hcri' cs
                     where hcri' [] = Left (PlayerDead "You drowned!")
                           hcri' (s:ss) = case s of Right _ -> s
                                                    Left (PlayerDead "You got eaten by a croc!") -> s
                                                    otherwise -> hcri' ss

hitCheckGoals :: Frogger -> [Goal] -> GameState
hitCheckGoals f gs = if or $ map (goalCollision f) gs
                        then LevelComplete
                     else Playing

roadCollision :: Frogger -> RoadMover -> GameState
roadCollision f r = if hasCollided f r then PlayerDead "You got hit by a car!"
                                       else Playing

riverCollision :: Frogger -> RiverMover -> Either GameState Float
riverCollision f r = if hasCollided f r then Right (getdX r)
                                        else Left $ PlayerDead "You drowned!"

goalCollision :: Frogger -> Goal -> Bool
goalCollision = hasCollided

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
