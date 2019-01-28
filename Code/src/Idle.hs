module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

-- TODO: Use Either to feed back either just the game state or the game state plus the new velocity of the frogger if it's on the back of something

idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            case (gameState e') of Playing -> e $~! \_ -> updateEnv e'
                                   otherwise -> return ()
            postRedisplay Nothing

updateEnv e = let roadEnemies' = map update (roadEnemies e)
                  riverEnemies' = map update (riverEnemies e)
                  (f_V', gameState') = case hitCheck e of Left gs   -> (0.0, gs)
                                                          Right vgs -> vgs
                  gameScore' = let s = gameScore e
                                in if gameState' == LevelComplete
                                      then s + 1000
                                   else s
                  frogger' = player e
               in e {player = frogger' {f_X = f_X frogger' + f_V'
                                       ,f_V = f_V'}
                    ,roadEnemies = roadEnemies'
                    ,riverEnemies = riverEnemies'
                    ,gameState = gameState'
                    ,gameScore = gameScore'
                    }

endLevel :: Env -> Env
endLevel e = e {gameState = LevelComplete
               ,gameScore = gameScore e + 1000}

hitCheck :: Env -> Either GameState (Float, GameState)
hitCheck e = let frogger = player e
                 fx = f_X frogger
                 fy = f_Y frogger
              in if inRange (0,initSizeX) fx && inRange (0,initSizeY) fy
                 then if inRange ((head lanes),(lanes !! 4)) fy -- On the road, death is the edge case
                         then let collisions = map (roadCollision frogger) (roadEnemies e)
                                  newState = roadCheck' collisions
                               in newState
                      else if inRange ((lanes !! 6),(lanes !! 11)) fy -- On the river, death is the default case
                         then let collisions = map (riverCollision frogger) (riverEnemies e)
                                  newState = riverCheck' collisions
                               in newState
                      else let collisions = map (goalCollision frogger) (goals e)
                            in if or collisions then Left LevelComplete
                                                else Left Playing
                 else Left (PlayerDead "You moved out of bounds!")
                 where roadCheck' [] = Left Playing
                       roadCheck' (s:ss) = case s of PlayerDead _ -> Left s
                                                     otherwise    -> roadCheck' ss
                       riverCheck' [] = Left (PlayerDead "You drowned!")
                       riverCheck' (s:ss) = case s of Right _ -> s
                                                      Left (PlayerDead "You got eaten by a croc!") -> s
                                                      otherwise -> riverCheck' ss

inRange :: Ord a => (a,a) -> a -> Bool
inRange (l,u) n = u >= n && n >= l

roadCollision :: Frogger -> RoadMover -> GameState
roadCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Car {ro_X = cx, ro_Y = cy, ro_L = cl, ro_W = cw})
  = if hasCollided2 fx fy fs cx cy cl cw then PlayerDead "You got hit by a car!"
                                         else Playing

riverCollision :: Frogger -> RiverMover -> Either GameState (Float, GameState)
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Croc {ri_X = cx, ri_Y = cy, ri_L = cl, ri_W = cw, ri_V = cv})
  = if hasCollided2 fx fy fs cx cy cl cw then Right (cv, Playing)
                                         else Left $ PlayerDead "You drowned!"
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Turtles {ri_X = tx, ri_Y = ty, ri_L = tl, ri_W = tw, ri_V = tv})
  = if hasCollided2 fx fy fs tx ty tl tw then Right (tv, Playing)
                                         else Left $ PlayerDead "You drowned!"
riverCollision (Frogger {f_X = fx, f_Y = fy, f_S = fs}) (Log {ri_X = lx, ri_Y = ly, ri_L = ll, ri_W = lw, ri_V = lv})
  = if hasCollided2 fx fy fs lx ly ll lw then Right (lv, Playing)
                                         else Left $ PlayerDead "You drowned!"

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

test :: Either Int String
test = Right "Hello"
