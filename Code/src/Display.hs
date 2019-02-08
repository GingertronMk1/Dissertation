-- |Module: Frogger.Display
module Display where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Type

-- |A function to draw an 'Env' to the screen.
gameDisplay :: Env -> Picture
gameDisplay e@E{sWidth = sw, sHeight = sh} = scale (sw/640) (sh/480) . translate (-320) (-240) $ drawGame e

drawGame :: Env -> Picture
drawGame e@E{gameState = gs} = case gs of PreStart          -> Pictures [
                                                                         translate 0 320 . textDraw $ "Welcome to Functional Frogger!"
                                                                        ,translate 0 300 . textDraw $ "W, A, S, and D are your movement keys!"
                                                                        ,translate 0 280 . textDraw $ "Space to pause, and ESC to quit!"
                                                                        ,translate 0 260 . textDraw $ "Press any key to start!"
                                                                        ]
                                          PlayerDead cause  -> Pictures [
                                                                         translate 0 320 . textDraw $ cause
                                                                        ,translate 0 300 . textDraw $ "You died with " ++ show (gameScore e) ++ " points!"
                                                                        ,translate 0 280 . textDraw $ "Press space to play again!"
                                                                        ]
                                          LevelComplete     -> Pictures [
                                                                         translate 0 320 . textDraw $ "You completed level " ++ show (level e) ++ " with " ++ show (gameScore e) ++" points!"
                                                                        ,translate 0 300 . textDraw $ "Press space to advance to level " ++ show (level e + 1) ++ "!"
                                                                        ]
                                          otherwise         -> Pictures [
                                                                         drawRoads
                                                                        ,drawRiver
                                                                        ,translate 0 440 . textDraw $ "Level " ++ show (level e) ++ ", " ++ show (gameScore e) ++ " points"
                                                                        ,translate 400 440 . textDraw . show . round $  time e
                                                                        ,draws $ riverEnemies e
                                                                        ,draws $ roadEnemies e
                                                                        ,draws $ goals e
                                                                        ,draw $ player e
                                                                        ,drawSides
                                                                        ]
                               where textDraw = Color white . Scale 0.1 0.1 . Text

drawRoads :: Picture
drawRoads = Pictures [Color (greyN 0.5) $ drawLane n | n <- [0..4]]

drawRiver :: Picture
drawRiver = Pictures [Color blue $ drawLane n | n <- [6..10]]

drawLane :: Lane -> Picture
drawLane n = translate 0 (lanes!!n) . Scale 640 30 $ unitSquare

drawSides :: Picture
drawSides = let w = 200
             in Pictures [translate (640) 0 . scale w 480 $ unitSquare
                         ,translate (-w) 0 . scale w 480 $ unitSquare]
