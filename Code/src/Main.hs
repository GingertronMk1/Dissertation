-- |Module: Frogger.Main
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import System.Environment
import Display
import Update
import Input
import Type

-- |'main' takes the arguments given on program launch and creates the initial environment.
--  It then creates a window of size given by the values 'initSizeX' and 'initSizeY' stored in 'Type'.
main :: IO()
main = do argc <- getArgs
          (initX,initY) <- getScreenSize
          let sH = fromIntegral initY
              sW = 4 * (sH/3)
              startLevel = (startEnv 1) {sWidth = sW
                                        ,sHeight = sH}
          putStrLn $ show argc
          putStrLn $ "x: " ++ show initX ++ ", y: " ++ show initY
          putStrLn $ "width: " ++ show sW ++ ", height: " ++ show sH
          play
            FullScreen      -- Play the game in a fullscreen window
            black           -- The background should be black
            60              -- The game should update 60 times per second
            startLevel      -- The first level
            gameDisplay     -- The function that draws a game
            gameInput       -- The function that passes input through
            gameUpdate      -- The function that updates the game
