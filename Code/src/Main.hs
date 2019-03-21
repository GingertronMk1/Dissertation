-- | Module: Frogger.Main

module Main where

-- My module imports
import Display
import Update
import Input
import Type

-- "External" imports
import Graphics.Gloss
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
          let allSpritesData = (\(Bitmap b) -> b) allSprites
              bg                = defSprite (384,600-448) (416,448) allSpritesData
              frLanded          = ("Frogger", ("landed", defSprite (0,600-32) (32,32) allSpritesData))
              frJumping         = ("Frogger", ("jumping", defSprite (160,600-32) (32,32) allSpritesData))
              car               = ("Car", ("n/a", defSprite (0,600-64) (32,32) allSpritesData))
              longlog           = ("Log", ("n/a", defSprite (0, 600-160) (128,32) allSpritesData))
              goal              = ("Goal", ("n/a", defSprite (0, 600-192) (32,32) allSpritesData))
              goalOcc           = ("Goal", ("occupied", Pictures $ map (snd . snd) [goal, frLanded]))
              croc              = ("Croc", ("n/a", defSprite (0,600-288) (128,32) allSpritesData))
              turtlesSurfaced   = ("Turtles", ("surfaced", defSprite (0,600-224) (96,32) allSpritesData))
              turtlesSubmerged  = ("Turtles", ("submerged", defSprite (192, 600-224) (96,32) allSpritesData))
              initSpriteList    = [frLanded
                                  ,frJumping
                                  ,car
                                  ,longlog
                                  ,goal
                                  ,goalOcc
                                  ,croc
                                  ,turtlesSurfaced
                                  ,turtlesSubmerged
                                  ]
          tSeed <- getCurrentTime >>= return                              -- Return that value
                                    . (\n -> div n $ 10^(12 :: Integer))  -- Divide by 10^12 to get the number of seconds
                                    . fromIntegral                        -- Convert from an Integer to an Int
                                    . diffTimeToPicoseconds               -- Convert from 'difftime' to an Integer we can use (picoseconds since midnight)
                                    . utctDayTime                         -- Get the current time of day in seconds
          let r  = mkStdGen tSeed
              sW = 416 :: Int
              sH = 448 :: Int
              startLevel = assignAllSprites $ (startEnv (fromIntegral sW) (fromIntegral sH) r) {background = bg, spriteList = initSpriteList}
          putStrLn $ show argc
          putStrLn $ show tSeed
          putStrLn . show . getSprites $ player startLevel
          printSpeeds $ roadEnemies startLevel
          printSpeeds $ riverEnemies startLevel
          play
            (InWindow "Frogger" (sW,sH) (0,0)) -- Play the game in a fullscreen window
            black             -- The background should be black
            60                -- The game should update 60 times per second
            startLevel -- The first level
            gameDisplay       -- The function that draws a game
            gameInput         -- The function that passes input through
            gameUpdate        -- The function that updates the game
        where defSprite pxy sxy d = BitmapSection (Rectangle {rectPos = pxy, rectSize = sxy}) d

-- | A function to print the dX values of a list of Drawables
printSpeeds :: Drawable a => [a] -> IO ()
printSpeeds = putStrLn                                -- Print it
            . show                                    -- Show that value
            . map (getdX . head)                      -- Get the speed of "all of them"
            . groupBy (\x1 x2 -> getY x1 == getY x2)  -- Group by y values (the lane)
