module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type
import Data.List (sortBy)
import Data.Ord (comparing)

idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            if hitCheck e'
            then e $~! \env -> env {gameState = PlayerDead}
            else if gameState e' == Playing
            then do e $~! \env -> let es = enemies env
                                  in env {enemies = map updateMover es}
            else return ()
            postRedisplay Nothing

hitCheck :: Env -> Bool
hitCheck = or . hitCheck'

hitCheck' :: Env -> [Bool]
hitCheck' env = map (hasCollided (player env)) (enemies env)

updateMover :: Mover -> Mover
updateMover c@(Car {x = cx, v = cv}) = c {x = cx + cv}
updateMover c@(Croc {x = cx, v = cv}) = c {x = cx + cv}
updateMover t@(Turtles{x = tx, v = tv}) = t {x = tx + tv}
updateMover l@(Log{x = lx, v = lv}) = l {x = lx + lv}

hasCollided :: Mover -> Mover -> Bool
hasCollided f m = case signum (l m) of 1         -> x m + l m > x f &&
                                                    y m + w m > y f &&
                                                    x f + s f > x m &&
                                                    y f + s f > y m
                                       -1        -> x m > x f &&
                                                    y m + w m > y f &&
                                                    x f + s f > x m + l m &&
                                                    y f + s f > y m
                                       otherwise -> False
