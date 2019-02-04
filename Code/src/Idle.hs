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
               in e {player = frogger' {f_V = f_V'}
                    ,roadEnemies = roadEnemies'
                    ,riverEnemies = riverEnemies'
                    ,gameState = gameState'
                    ,gameScore = gameScore'
                    }

hitCheck :: Env -> Either GameState Float
hitCheck e = let frogger = player e
                 fx = f_X frogger
                 fy = f_Y frogger
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
roadCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Car {ro_X = cx, ro_Y = cy, ro_L = cl, ro_W = cw})
  = if hasCollided fx fy fs cx cy cl cw then PlayerDead "You got hit by a car!"
                                        else Playing

riverCollision :: Frogger -> RiverMover -> Either GameState Float
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Croc {ri_X = cx, ri_Y = cy, ri_L = cl, ri_W = cw, ri_V = cv})
  = if hasCollided fx fy fs cx cy cl cw then Right cv
                                        else Left $ PlayerDead "You drowned!"
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Turtles {ri_X = tx, ri_Y = ty, ri_L = tl, ri_W = tw, ri_V = tv})
  = if hasCollided fx fy fs tx ty tl tw then Right tv
                                        else Left $ PlayerDead "You drowned!"
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Log {ri_X = lx, ri_Y = ly, ri_L = ll, ri_W = lw, ri_V = lv})
  = if hasCollided fx fy fs lx ly ll lw then Right lv
                                        else Left $ PlayerDead "You drowned!"

goalCollision :: Frogger -> Goal -> Bool
goalCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Goal {g_X = gx, g_Y = gy, g_S = gs})
  = if hasCollided fx fy fs gx gy gs gs then True
                                        else False

hasCollided :: Float -> Float -> Float ->           -- Frogger x, y, and size
               Float -> Float -> Float -> Float ->  -- Enemy x, y, length, width
               Bool                                 -- Have the two collided?
hasCollided fx fy fs ex ey el ew = case signum el of 1         -> ex + el > fx &&
                                                                  ey + ew > fy &&
                                                                  fx + fs > ex &&
                                                                  fy + fs > ey
                                                     -1        -> ex > fx &&
                                                                  ey + ew > fy &&
                                                                  fx + fs > ex + el &&
                                                                  fy + fs > ey
                                                     otherwise -> False

test :: Either Int String
test = Right "Hello"
