-- |Module: Frogger.Main
module Main where

import Graphics.UI.GLUT
import Data.IORef
import Display
import Idle
import Input
import Type

-- |'main' takes the arguments given on program launch and creates the initial environment.
--  It then creates a window of size given by the values 'initSizeX' and 'initSizeY' stored in 'Type'.
main :: IO()
main = do (_progName, _args) <- getArgsAndInitialize
          putStrLn (_progName ++ " " ++ concat _args)
          initialDisplayMode $= [DoubleBuffered]
          let iX = round initSizeX :: GLsizei
              iY = round initSizeY :: GLsizei
          initialWindowSize $= Size iX iY
          createWindow "Frogger"
          reshapeCallback $= Just reshape
          f <- newIORef $ startEnv 1
          keyboardMouseCallback $= Just (input f)
          displayCallback $= display f
          idleCallback $= Just (idle f)
          mainLoop


-- |'reshape' is the reshapeCallback, which is called when the window is resized
reshape size = do viewport $= (Position 0 0, size)
                  postRedisplay Nothing
