-- | Module: Frogger.Main
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

-- | 'main' takes the arguments given on program launch and (currently) only prints them
--   It then takes the height and width of the window, creates an initial Env with those, and starts the game
main :: IO()
main = do argc <- getArgs
          (initX,initY) <- getScreenSize
          let sH = fromIntegral initY
              sW = 4 * (sH/3)
              startLevel = startEnv {sWidth = sW ,sHeight = sH}
          putStrLn $ show argc
          play
            FullScreen      -- Play the game in a fullscreen window
            black           -- The background should be black
            60              -- The game should update 60 times per second
            startLevel      -- The first level
            gameDisplay     -- The function that draws a game
            gameInput       -- The function that passes input through
            gameUpdate      -- The function that updates the game
