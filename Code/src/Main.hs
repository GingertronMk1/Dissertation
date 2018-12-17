module Main where

import Graphics.UI.GLUT
import Display

main :: IO()
main = do (_progName, _args) <- getArgsAndInitialize
          putStrLn $ _progName ++ " " ++ concat _args
          initialWindowSize $= Size 640 480
          createWindow "Frogger"
          displayCallback $= display
          mainLoop
