module Main where

import Graphics.UI.GLUT
import Data.IORef
import Display
import Type
import Input

main :: IO()
main = do (_progName, _args) <- getArgsAndInitialize
          putStrLn $ _progName ++ " " ++ concat _args
          initialDisplayMode $= [DoubleBuffered]
          initialWindowSize $= Size 640 480
          createWindow "Frogger"
          reshapeCallback $= Just reshape
          f <- newIORef $ Frogger {x = 320.0, y = 240.0, s = 10.0}
          keyboardMouseCallback $= Just (input f)
          displayCallback $= display f
          idleCallback $= Just (idle f)
          mainLoop

idle e = do e $~! id
            postRedisplay Nothing

reshape size = do viewport $= (Position 0 0, size)
                  postRedisplay Nothing
