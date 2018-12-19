module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

idle :: IORef Env -> IdleCallback
idle e = do e' <- get e
            hitCheck e'
            if gameState e' == Playing
            then do e $~! \env -> let es = enemies env
                                  in env {enemies = map updateMover es}
                    postRedisplay Nothing
            else postRedisplay Nothing

hitCheck :: Env -> IO()
hitCheck env = if or $ map (hasCollided (player env)) (enemies env)
               then putStrLn "Hit!"
               else return()

updateMover :: Mover -> Mover
updateMover c@(Car {x = cx, v = cv}) = c {x = cx + cv}
updateMover c@(Croc {x = cx, v = cv}) = c {x = cx + cv}
updateMover t@(Turtles{x = tx, v = tv}) = t {x = tx + tv}
updateMover l@(Log{x = lx, v = lv}) = l {x = lx + lv}

hasCollided :: Mover -> Mover -> Bool
{-
hasCollided (Frogger {x=fx, y=fy, s=fs}) (Car {x=cx, y=cy, l=cl, w=cw, v=cv})
  = let froggerBoundsX = (fx,fx+fs)
        froggerBoundsY = (fy,fy+fs)
        carBoundsX     = (cx,cx+(cl*signum cv))
        carBoundsY     = (cy,cy+cw)
    in (inRange froggerBoundsX (fst carBoundsX) || inRange froggerBoundsX (snd carBoundsX))
       && (inRange froggerBoundsY (fst carBoundsY) || inRange froggerBoundsY (snd carBoundsY))
       -}
hasCollided f m = let froggerBoundsX = (x f, x f + s f)
                      froggerBoundsY = (y f, y f + s f)
                      moverBoundsX     = (x m, x m+(l m*(signum (v m))))
                      moverBoundsY     = (y m, y m+w m)
                  in (inRange froggerBoundsX (fst moverBoundsX) || inRange froggerBoundsX (snd moverBoundsX))
                  && (inRange froggerBoundsY (fst moverBoundsY) || inRange froggerBoundsY (snd moverBoundsY))


inRange :: (Num a, Ord a) => (a, a) -> a -> Bool
inRange (m, n) p = if m < n then m <= p && p <= n
                            else n <= p && p <= m
