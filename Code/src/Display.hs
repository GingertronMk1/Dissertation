-- |Module: Frogger.Display
module Display where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Type

-- |A function to draw an 'Env' to the screen.
--  It scales and translates the 'drawGame' function to the correct window size.
gameDisplay :: Env -> Picture
gameDisplay e@E{sWidth = sw, sHeight = sh} = scale (sw/4000) (sh/3000) . translate (-2000) (-1500) $ drawGame e

-- | drawGame draws the game at a resolution of 4000x3000
--   This makes a great deal of the maths involved in the game logic considerably simpler, and the result can simply be scaled down to the actual window size.
drawGame :: Env -> Picture
drawGame e@E{gameState = gs} = case gs of PreStart          -> Pictures [
                                                                         translate 0 1800 . textDraw $ "Welcome to Functional Frogger!"
                                                                        ,translate 0 1600 . textDraw $ "W, A, S, and D are your movement keys!"
                                                                        ,translate 0 1400 . textDraw $ "Space to pause, and ESC to quit!"
                                                                        ,translate 0 1200 . textDraw $ "Press any key to start!"
                                                                        ]
                                          PlayerDead cause  -> Pictures [
                                                                         translate 0 1700 . textDraw $ cause
                                                                        ,translate 0 1500 . textDraw $ "You died with " ++ show (gameScore e) ++ " points!"
                                                                        ,translate 0 1300 . textDraw $ "Press space to play again!"
                                                                        ]
                                          LevelComplete     -> Pictures [
                                                                         translate 0 1600 . textDraw $ "You completed level " ++ show (level e) ++ " with " ++ show (gameScore e) ++" points!"
                                                                        ,translate 0 1400 . textDraw $ "Press space to advance to level " ++ show (level e + 1) ++ "!"
                                                                        ]
                                          otherwise         -> Pictures [
                                                                         drawVerge
                                                                        ,drawRoads
                                                                        ,drawRiver
                                                                        ,translate 0 2850 . textDraw $ "Level " ++ show (level e) ++ ", " ++ show (gameScore e) ++ " points"
                                                                        ,translate 3000 2850 . textDraw . show . round $  time e
                                                                        ,draws $ riverEnemies e
                                                                        ,draws $ roadEnemies e
                                                                        ,draws $ goals e
                                                                        ,draw $ player e
                                                                        ,drawSides (sWidth e)
                                                                        ]
                               where textDraw = Color white . Text

-- | Drawing the verge at the top of the screen that the Froggers call home.
drawVerge :: Picture
drawVerge = Color green . translate 0 (lanes!!12) . Scale 4000 400 $ unitSquare

-- | Drawing the lanes of the Road.
drawRoads :: Picture
drawRoads = Pictures [Color (greyN 0.5) $ drawLane n | n <- [0..4]]

-- | Drawing the lanes of the River.
drawRiver :: Picture
drawRiver = Pictures [Color blue $ drawLane n | n <- [6..11]]

-- | A function to draw a lane - a rectangle of height 190 that fills the width of the screen.
drawLane :: Lane -> Picture
drawLane n = translate 0 (lanes!!n + 5) . Scale 4000 190 $ unitSquare

-- | Drawing the sides of the map - the moving objects go beyond the width of the lanes and this hides them from view.
drawSides :: Float -> Picture
drawSides w = let w' = 0.5 * (4000-w)
               in Pictures [translate (4000) 0 . scale w' 3000 $ unitSquare
                           ,translate (-w') 0 . scale w' 3000 $ unitSquare]
