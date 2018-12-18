module Idle (idle) where

import Graphics.UI.GLUT
import Data.IORef
import Type

idle :: IORef Env -> IdleCallback
idle e = do e $~! \env -> let es = enemies env
                          in env {enemies = map updateMover es}
            e' <- get e
            putStrLn $ show e'
            postRedisplay Nothing

updateMover :: Mover -> Mover
updateMover c@(Car {x = cx, v = cv}) = c {x = cx + cv}
