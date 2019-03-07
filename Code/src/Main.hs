-- | Module: Frogger.Main
module Main where

-- My module imports
import Display
import Update
import Input
import Type

-- "External" imports
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import System.Environment
import System.Random
import Data.List
import Data.Time.Clock

-- | 'main' takes the arguments given on program launch and (currently) only prints them
--   It then takes the height and width of the window, creates an initial Env with those, and starts the game
main :: IO ()
main = do argc <- getArgs
          putStrLn $ show lanes
          allSprites <- loadBMP "img/frogger_sprites.bmp"
          putStrLn . show . bitmapSize $ (\(Bitmap b) -> b) allSprites
          (_,initY) <- getScreenSize
          tSeed <- getCurrentTime >>= return                              -- Return that value
                                    . (\n -> div n $ 10^(12 :: Integer))  -- Divide by 10^12 to get the number of seconds
                                    . fromIntegral                        -- Convert from an Integer to an Int
                                    . diffTimeToPicoseconds               -- Convert from 'difftime' to an Integer we can use (picoseconds since midnight)
                                    . utctDayTime                         -- Get the current time of day in seconds
          let sH = fromIntegral initY
              sW = 4 * (sH/3)
              r  = mkStdGen tSeed
              startLevel = (startEnv sW sH r) {spriteMap = allSprites}
          putStrLn $ show argc
          putStrLn $ show tSeed
          printSpeeds $ roadEnemies startLevel
          printSpeeds $ riverEnemies startLevel
          play
            FullScreen      -- Play the game in a fullscreen window
            black           -- The background should be black
            60              -- The game should update 60 times per second
            startLevel      -- The first level
            gameDisplay     -- The function that draws a game
            gameInput       -- The function that passes input through
            gameUpdate      -- The function that updates the game

-- | A function to print the dX values of a list of Drawables
printSpeeds :: Drawable a => [a] -> IO ()
printSpeeds = putStrLn                                -- Print it
            . show                                    -- Show that value
            . map (getdX . head)                      -- Get the speed of "all of them"
            . groupBy (\x1 x2 -> getY x1 == getY x2)  -- Group by y values (the lane)
