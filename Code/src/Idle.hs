module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            case (gameState e') of Playing -> e $~! \e' -> updateEnv e'
                                   otherwise -> return ()
            postRedisplay Nothing

updateEnv :: Env -> Env
updateEnv e = let frogger' = update (player e)
                  roadEnemies' = map update (roadEnemies e)
                  riverEnemies' = map update (riverEnemies e)
                  (f_V', gameState') = case hitCheck e of Left gs -> (0.0, gs)
                                                          Right v -> (v, Playing)
                  gameScore' = let s = gameScore e
                                in if gameState' == LevelComplete
                                      then s + (1000 * level e)
                                   else s
               in e {player = setdX f_V' frogger'
                    ,roadEnemies = roadEnemies'
                    ,riverEnemies = riverEnemies'
                    ,gameState = gameState'
                    ,gameScore = gameScore'
                    }

hitCheck :: Env -> Either GameState Float
hitCheck e = let frogger = player e
                 fx = getX frogger
                 fy = getY frogger
              in if inRange (0,initSizeX) fx && inRange (0,initSizeY) fy
                 then if inRange ((head lanes),(lanes !! 4)) fy
                         then Left $ hitCheckRoad frogger (roadEnemies e)
                      else if inRange ((lanes !! 6),(lanes !! 11)) fy
                         then hitCheckRiver frogger (riverEnemies e)
                      else let collisions = map (goalCollision frogger) (goals e)
                            in Left $ hitCheckGoals frogger (goals e)
                 else Left $ PlayerDead "You moved out of bounds!"
                 where inRange (l,u) n = u >= n && n >= l

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
