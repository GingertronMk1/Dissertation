module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

idle :: IORef Env -> IdleCallback
idle e = do e $~! \env -> let es = enemies env
                          in env {enemies = map updateMover es}
            e' <- get e
            hitCheck e'
            --putStrLn $ show e'
            postRedisplay Nothing

hitCheck :: Env -> IO()
hitCheck env = if or $ map (hasCollided (player env)) (enemies env)
               then putStrLn "Hit!"
               else return()

updateMover :: Mover -> Mover
updateMover c@(Car {x = cx, v = cv}) = c {x = cx + cv}

hasCollided :: Mover -> Mover -> Bool
hasCollided (Frogger {x=fx, y=fy, s=fs}) (Car {x=cx, y=cy, l=cl, w=cw, v=cv})
  = let froggerBoundsX = (fx,fx+fs)
        froggerBoundsY = (fy,fy+fs)
        carBoundsX     = (cx,cx+(cl*signum cv))
        carBoundsY     = (cy,cy+cw)
    in (inRange froggerBoundsX (fst carBoundsX) || inRange froggerBoundsX (snd carBoundsX))
       && (inRange froggerBoundsY (fst carBoundsY) || inRange froggerBoundsY (snd carBoundsY))

inRange :: (Num a, Ord a) => (a, a) -> a -> Bool
inRange (m, n) p = if m < n then m <= p && p <= n
                            else n <= p && p <= m
