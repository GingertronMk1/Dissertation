-- | Module: Frogger.Display
module Display where

import Graphics.Gloss
import Type

-- | A function to draw an 'Env' to the screen.
--   It scales and translates the 'drawGame' function to the correct window size.
gameDisplay :: Env -> Picture
gameDisplay e@E{sWidth = sw, sHeight = sh} = scale (sw/4000) (sh/3000) . translate (-2000) (-1500) $ drawGame e

-- | drawGame draws the game at a resolution of 4000x3000
--   This makes a great deal of the maths involved in the game logic considerably simpler, and the result can simply be scaled down to the actual window size.
drawGame :: Env -> Picture
drawGame e@E{gameState = gs}
  = case gs of PreStart      -> Pictures [
                                          translate 0 1800 . textDraw $ "Welcome to Frogger!"
                                         ,translate 0 1600 . textDraw $ "W, A, S, and D are your movement keys!"
                                         ,translate 0 1400 . textDraw $ "Space to pause, and ESC to quit!"
                                         ,translate 0 1200 . textDraw $ "Press any key to start!"
                                         ]
               PlayerDead c  -> Pictures [
                                          translate 0 1700 . textDraw $ c
                                         ,translate 0 1500 . textDraw $ "You died with "
                                                                     ++ show (gameScore e)
                                                                     ++ " points!"
                                         ,translate 0 1300 . textDraw $ "Press space to play again!"
                                         ]
               LevelComplete -> Pictures [
                                          translate 0 1600 . textDraw $ "You completed level "
                                                                     ++ show (level e)
                                                                     ++ " with "
                                                                     ++ show (gameScore e)
                                                                     ++" points!"
                                         ,translate 0 1400 . textDraw $ "Press space to advance to level "
                                                                     ++ show (level e + 1)
                                                                     ++ "!"
                                         ]
               _             -> Pictures [
                                          translate 2000 1500 . scale (4000/416) (3000/448) $ background e
                                         ,translate 0 2850 . textDraw $ "Level "
                                                                     ++ show (level e)
                                                                     ++ ", "
                                                                     ++ show (gameScore e)
                                                                     ++ " points"
                                         ,translate 3000 2850 . textDraw . show . (\n -> round n :: Int) $  time e
                                         ,draws $ riverEnemies e
                                         ,draws $ roadEnemies e
                                         ,draws $ goals e
                                         ,draw $ player e
                                         ,drawSides
                                         ]
    where textDraw = color white . Text

-- | Drawing the verge at the top of the screen that the Froggers call home.
drawHome :: Picture
drawHome = color green . translate 2000 (lanes!!14) $ rectangleSolid 4000 600

-- | Drawing the verge between Road and River
drawVerge :: Picture
drawVerge = color (makeColor 0.85 0.65 0.45 1.0) . translate 2000 (lanes!!6) $ rectangleSolid 4000 300

-- | Drawing the lanes of the Road.
drawRoads :: Picture
drawRoads = Pictures [color (greyN 0.5) $ drawLane n | n <- [1..5]]

-- | Drawing the lanes of the River.
drawRiver :: Picture
drawRiver = Pictures [color blue $ drawLane n | n <- [7..12]]

-- | A function to draw a lane - a rectangle of height 190 that fills the width of the screen.
drawLane :: Lane -> Picture
drawLane n = translate 2000 (lanes!!n + 5) $ rectangleSolid 4000 190

-- | Drawing the sides of the map - the moving objects go beyond the width of the lanes and this hides them from view.
drawSides :: Picture
drawSides = let wi = 2000
             in color black $ Pictures [translate (0-(wi/2)) 1500 $ rectangleSolid wi 3000
                                       ,translate (4000+(wi/2)) 1500 $ rectangleSolid wi 3000]


