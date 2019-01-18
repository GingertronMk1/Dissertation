module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            if hitCheck (player e') (enemies e')
            then e $~! \env -> env {gameState = PlayerDead "You hit an enemy"}
            else if hitCheck (player e') (goals e')
            then if gameState e' /= LevelComplete
                 then e $~! \env -> env {gameState = LevelComplete
                                        , gameScore = gameScore env + 1000}
                 else return ()
            else if gameState e' == Playing
            then do e $~! \env -> let es = enemies env
                                  in env {enemies = map updateMover es}
            else return ()
            postRedisplay Nothing

hitCheck :: Mover -> [Mover] -> Bool
hitCheck f ms = or $ hitCheck' f ms

hitCheck' :: Mover -> [Mover] -> [Bool]
hitCheck' f ms = map (hasCollided f) ms

updateMover :: Mover -> Mover
updateMover c@(Car {x = cx, v = cv})    = c {x = cx + cv}
updateMover c@(Croc {x = cx, v = cv})   = c {x = cx + cv}
updateMover t@(Turtles{x = tx, v = tv}) = t {x = tx + tv}
updateMover l@(Log{x = lx, v = lv})     = l {x = lx + lv}

hasCollided :: Mover -> Mover -> Bool
hasCollided f c@(Car {})      = hasCollided2 f c
hasCollided f c@(Croc {})     = hasCollided2 f c
hasCollided f t@(Turtles {})  = hasCollided2 f t
hasCollided f l@(Log {})      = hasCollided2 f l
hasCollided f g@(Goal {})     = hasCollided1 f g

hasCollided1 :: Mover -> Mover -> Bool
hasCollided1 f m = x m + s m > x f &&
                   y m + s m > y f &&
                   x f + s f > x m &&
                   y f + s f > y m

hasCollided2 :: Mover -> Mover -> Bool
hasCollided2 f m = case signum (l m) of 1         -> x m + l m > x f &&
                                                     y m + w m > y f &&
                                                     x f + s f > x m &&
                                                     y f + s f > y m
                                        -1        -> x m > x f &&
                                                     y m + w m > y f &&
                                                     x f + s f > x m + l m &&
                                                     y f + s f > y m
                                        otherwise -> False
