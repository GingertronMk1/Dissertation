-- |Module: Frogger.Update
module Update where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Data.Maybe
import Type

-- |The UpdateCallback function.
--  This is responsible for updating the game as it goes.
--  Only update the game while it is in the "Playing" state.
gameUpdate :: Float   -- ^ Delta-t in milliseconds
              -> Env  -- ^ The current Env
              -> Env  -- ^ The updated Env
gameUpdate n e@E{gameState = gs, time = t} = if gs == Playing then updateEnv $ e {time = t + n}
                                                              else e

-- |'updateEnv' is a composition of 3 functions which update the positions of moving objects, detect collision between the player and those objects, detect a collision between the player and a goal, and update the score respectively.
updateEnv :: Env -> Env
updateEnv e = let p = player e
                  coll = if is_Jumping p
                         then id
                         else hitCheck
               in scoreUpdate . coll . updateMovers $ e


-- |'scoreUpdate' is a simple function that, if the level is complete, increases the score by an amount proportional to the current level
scoreUpdate :: Env -> Env
scoreUpdate e = if gameState e == LevelComplete then e {gameScore = gameScore e + (1000 * level e)}
                                                else e

-- |'updateMovers' simply applies the 'update' function required of all 'Drawable's to all moving objects
updateMovers :: Env -> Env
updateMovers e = e {player = update . player $ e
                   ,roadEnemies = map update . roadEnemies $ e
                   ,riverEnemies = map update . riverEnemies $ e
                   }

-- |hitCheck acts as something of a routing function; provided the player is within bounds it calls the relevant function depending upon where on the map they are.
--  If they are on the Road, it calls 'roadCheck' and conversely it calls 'riverCheck' if they are on the River.
--  This is in the interest of efficiency; there is no point checking if the player has collided with something that does not exist on their part of the map.
hitCheck :: Env -> Env
hitCheck e = let p = player e
                 px = getX p
                 py = getY p
              in if inRange (0, initSizeX) px && inRange (0, initSizeY) py
                    then if inRange (head lanes, lanes!!6) py      then roadCheck e
                         else if inRange (lanes!!6, lanes!!12) py  then riverCheck e
                         else                                           e
                 else e {gameState = PlayerDead "You went out of bounds!"}
            where inRange (l,u) n = case compare l u of LT -> u >= n && n >= l
                                                        EQ -> n == u
                                                        GT -> l >= n && n >= u

-- |roadCheck checks to see if the player has been run over yet.
--  If the player is hit by a car, they die.
roadCheck :: Env -> Env
roadCheck e = let p = player e
                  coll = lookup True . map (\m -> (hasCollided p m, m)) $ roadEnemies e
               in case coll of Just m   -> e {gameState = PlayerDead "You got run over!"}
                               Nothing  -> e {player = setdX 0 p}

-- |riverCheck performs the equal yet opposite function to roadCheck in that it checks to see if the player has saved themself by jumping onto something.
--  If the player has collided with either a RiverMover or a Goal they remain alive (well, may remain alive in the Goal case).
--  Otherwise they drown.
--  If the player has collided with a Goal the function 'hitGoal' is called to deal with that.
riverCheck :: Env -> Env
riverCheck e = let p = player e
                   collRi = lookup True . map (\m -> (hasCollided p m, m)) $ riverEnemies e
                   collGo = lookup True . map (\g -> (hasCollided p g, g)) $ goals e
                in case collRi of Just c@(Croc {}) -> if onCrocsHead p c
                                                      then e {gameState = PlayerDead "You got eaten by a crocodile!"}
                                                      else e {player = setdX (getdX c) p}
                                  Just m  -> e {player = setdX (getdX m) p}
                                  Nothing -> case collGo of Just g  -> hitGoal g e
                                                            Nothing -> e {gameState = PlayerDead "You drowned!"}
               where onCrocsHead p c = let cx = getX c
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
                                        in hasCollided p crocHead && not (hasCollided p crocBody)

-- |hitGoal deals with when a Goal has been collided with.
--  If that Goal wa occupied, the player dies.
--  Otherwise, that Goal is made occupied and the players position resets.
--  If all Goals are occupied, the level is complete.
hitGoal :: Goal -- ^ The Goal that has been collided with
        -> Env  -- ^ The current Env
        -> Env  -- ^ The resultant Env
hitGoal g e = let p = player e
                  gs = goals e
               in if is_Occupied g
                  then e {gameState = PlayerDead "That goal had someone on it!"}
                  else let gs' = g {is_Occupied = True} : filter (/=g) gs
                        in if all is_Occupied gs' then e {gameState = LevelComplete}
                                                  else e {player = newPlayer
                                                         ,goals = gs'
                                                         }

-- |Detecting whether or not the Frogger has collided with another Drawable
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
