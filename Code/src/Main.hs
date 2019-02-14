-- | Module: Frogger.Main
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import System.Environment
import System.Random
import Data.Time.Clock
import Display
import Update
import Input
import Type

-- | 'main' takes the arguments given on program launch and (currently) only prints them
--   It then takes the height and width of the window, creates an initial Env with those, and starts the game
main :: IO()
main = do argc <- getArgs
          (initX,initY) <- getScreenSize
          tSeed <- getCurrentTime >>= return . (\n -> mod n 1000) . (\n -> div n 1000000) . fromIntegral . diffTimeToPicoseconds . utctDayTime
          let sH = fromIntegral initY
              sW = 4 * (sH/3)
              r  = mkStdGen tSeed
              startLevel = startEnv sW sH r
          putStrLn $ show argc
          putStrLn $ show tSeed
          putStrLn $ show startLevel
          play
            FullScreen      -- Play the game in a fullscreen window
            black           -- The background should be black
            60              -- The game should update 60 times per second
            startLevel      -- The first level
            gameDisplay     -- The function that draws a game
            gameInput       -- The function that passes input through
            gameUpdate      -- The function that updates the game
