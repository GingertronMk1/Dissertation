module Display (display) where

import Graphics.UI.GLUT
import Data.IORef
import Data.List (intersperse)
import Type

display :: IORef Env -> DisplayCallback
display m = do fr <- get m
               clear [ColorBuffer]
               loadIdentity
               translate $ Vector3 (-1.0) (-1.0) (0.0 :: Float)
               scale (2.0/initSizeX) (2.0/initSizeY) (0.0 :: Float)
               color $ Color3 1.0 1.0 (1.0 :: Float)
               case (gameState fr) of PreStart         -> do let line1 = "Welcome to Functional Frogger!"
                                                                 line2 = "W, A, S, and D are your movement keys"
                                                                 line3 = "Space will pause the game"
                                                                 line4 = "Press any key to start!"
                                                             preservingMatrix $ do textTranslateScale
                                                                                   drawString line1
                                                                                   drawString line2
                                                                                   drawString line3
                                                                                   drawString line4
                                      PlayerDead cause -> do let score = "You died with " ++ show (gameScore fr) ++ " points!"
                                                                 lev   = "You got to level " ++ show (level fr) ++ "!"
                                                                 space = "Press space to play again!"
                                                             preservingMatrix $ do textTranslateScale
                                                                                   drawString cause
                                                                                   drawString lev
                                                                                   drawString score
                                                                                   drawString space
                                      LevelComplete    -> do let score = show $ gameScore fr
                                                                 state = "You completed the level with " ++ score ++ " points!"
                                                                 space = "Press space to advance to level " ++ show (level fr + 1) ++ "!"
                                                             preservingMatrix $ do textTranslateScale
                                                                                   drawString state
                                                                                   drawString space
                                      otherwise        -> do t <- get elapsedTime
                                                             let t = time fr
                                                                 ppState = show (gameState fr) ++ " Level " ++ show (level fr)
                                                                 ppScore = show $ gameScore fr
                                                                 ppTime  = show $ time fr
                                                              in preservingMatrix $ do translate $ Vector3 32.0 440.0 (0.0 :: Float)
                                                                                       preservingMatrix $ do scale 0.1 0.1 (1.0 :: Float)
                                                                                                             renderString MonoRoman $ ppState
                                                                                       translate $ Vector3 200.0 0.0 (0.0 :: Float)
                                                                                       preservingMatrix $ do scale 0.1 0.1 (1.0 :: Float)
                                                                                                             renderString MonoRoman $ ppScore
                                                                                       translate $ Vector3 200.0 0.0 (0.0 :: Float)
                                                                                       preservingMatrix $ do scale 0.1 0.1 (1.0 :: Float)
                                                                                                             renderString MonoRoman $ ppTime
                                                             sequence . map drawRoadLane . take 5 $ lanes
                                                             sequence . map drawRiverLane . take 5 . drop 6 $ lanes
                                                             preservingDraws . roadEnemies $ fr
                                                             preservingDraws . riverEnemies $ fr
                                                             preservingDraws . goals $ fr
                                                             preservingDraw . player $ fr
               swapBuffers
               where drawString s = do preservingMatrix $ renderString MonoRoman s
                                       translate $ Vector3 0.0 (-200.0) (0.0 :: Float)
                     textTranslateScale = do translate $ Vector3 32.0 240.0 (0.0 :: Float)
                                             scale 0.1 0.1 (1.0 :: Float)

drawRiverLane :: Float -> IO()
drawRiverLane y = preservingMatrix $ do color $ Color3 0.2 0.2 (1.0 :: Float)
                                        drawLane y

drawRoadLane :: Float -> IO()
drawRoadLane y = preservingMatrix $ do color $ Color3 0.3 0.3 (0.3 :: Float)
                                       drawLane y

drawLane :: Float -> IO()
drawLane y = do translate $ Vector3 0.0 y 0.0
                scale initSizeX 30.0 (1.0 :: Float)
                unitSquare
                where unitSquare = let us = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float, Float, Float)]
                                  in (renderPrimitive Quads . mapM_ makeVertex) us
                      makeVertex (x,y,z) = vertex $ Vertex3 x y z
