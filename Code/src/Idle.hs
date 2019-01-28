module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            case (gameState e') of Playing -> e $~! \_ -> updateEnv e'
                                   otherwise -> return ()
            postRedisplay Nothing

updateEnv e = let roadEnemies' = map update (roadEnemies e)
                  riverEnemies' = map update (riverEnemies e)
                  gameState' = hitCheck e
                  gameScore' = let s = gameScore e
                                in if gameState' == LevelComplete
                                      then s + 1000
                                   else s
               in e {roadEnemies = roadEnemies'
                    ,riverEnemies = riverEnemies'
                    ,gameState = gameState'
                    ,gameScore = gameScore'
                    }

endLevel :: Env -> Env
endLevel e = e {gameState = LevelComplete
               ,gameScore = gameScore e + 1000}

hitCheck :: Env -> GameState
hitCheck e = let frogger = player e
                 fx = f_X frogger
                 fy = f_Y frogger
              in if inRange (0,initSizeX) fx && inRange (0,initSizeY) fy
                 then if inRange ((head lanes),(lanes !! 4)) fy -- On the road, death is the edge case
                         then let collisions = map (roadCollision frogger) (roadEnemies e)
                               in roadCheck' collisions
                      else if inRange ((lanes !! 6),(lanes !! 11)) fy -- On the river, death is the default case
                         then let collisions = map (riverCollision frogger) (riverEnemies e)
                              in riverCheck' collisions
                      else let collisions = map (goalCollision frogger) (goals e)
                            in if or collisions then LevelComplete
                                                else Playing
                 else PlayerDead "You moved out of bounds!"
                 where roadCheck' [] = Playing
                       roadCheck' (s:ss) = case s of PlayerDead _ -> s
                                                     otherwise    -> roadCheck' ss
                       riverCheck' [] = PlayerDead "You drowned!"
                       riverCheck' (s:ss) = if s == Playing || s == PlayerDead "You got eaten by a croc!"
                                               then s
                                            else riverCheck' ss

inRange :: Ord a => (a,a) -> a -> Bool
inRange (l,u) n = u >= n && n >= l

roadCollision :: Frogger -> RoadMover -> GameState
roadCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Car {ro_X = cx, ro_Y = cy, ro_L = cl, ro_W = cw})
  = if hasCollided2 fx fy fs cx cy cl cw then PlayerDead "You got hit by a car!"
                                         else Playing

riverCollision :: Frogger -> RiverMover -> GameState
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Croc {ri_X = cx, ri_Y = cy, ri_L = cl, ri_W = cw})
  = if hasCollided2 fx fy fs cx cy cl cw then Playing
                                         else PlayerDead "You drowned!"
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Turtles {ri_X = tx, ri_Y = ty, ri_L = tl, ri_W = tw})
  = if hasCollided2 fx fy fs tx ty tl tw then Playing
                                         else PlayerDead "You drowned!"
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Log {ri_X = lx, ri_Y = ly, ri_L = ll, ri_W = lw})
  = if hasCollided2 fx fy fs lx ly ll lw then Playing
                                         else PlayerDead "You drowned!"

goalCollision :: Frogger -> Goal -> Bool
goalCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Goal {g_X = gx, g_Y = gy, g_S = gs})
  = if hasCollided1 fx fy fs gx gy gs then True
                                      else False

hasCollided1 :: Float -> Float -> Float ->  -- Frogger x, y, and size
                Float -> Float -> Float ->  -- Enemy x, y, and size
                Bool                        -- Have the two collided?
hasCollided1 fx fy fs ex ey es = ex + es > fx &&
                                 ey + es > fy &&
                                 fx + fs > ex &&
                                 fy + fs > ey

hasCollided2 :: Float -> Float -> Float ->           -- Frogger x, y, and size
                Float -> Float -> Float -> Float ->  -- Enemy x, y, length, width
                Bool                                 -- Have the two collided?
hasCollided2 fx fy fs ex ey el ew = case signum el of 1         -> ex + el > fx &&
                                                                   ey + ew > fy &&
                                                                   fx + fs > ex &&
                                                                   fy + fs > ey
                                                      -1        -> ex > fx &&
                                                                   ey + ew > fy &&
                                                                   fx + fs > ex + el &&
                                                                   fy + fs > ey
                                                      otherwise -> False
