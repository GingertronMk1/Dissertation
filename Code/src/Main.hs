module Main where

import Graphics.UI.GLUT
import Data.IORef
import Display
import Idle
import Input
import Type

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


reshape size = do viewport $= (Position 0 0, size)
                  postRedisplay Nothing
