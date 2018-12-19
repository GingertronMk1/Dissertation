module Main where

import Graphics.UI.GLUT
import Data.IORef
import Display
import Idle
import Input
import Type

main :: IO()
main = do (_progName, _args) <- getArgsAndInitialize
          putStrLn $ _progName ++ " " ++ concat _args
          initialDisplayMode $= [DoubleBuffered]
          initialWindowSize $= Size 640 480
          createWindow "Frogger"
          reshapeCallback $= Just reshape
          f <- newIORef $ startEnv
          keyboardMouseCallback $= Just (input f)
          displayCallback $= display f
          idleCallback $= Just (idle f)
          mainLoop


reshape size = do viewport $= (Position 0 0, size)
                  postRedisplay Nothing
