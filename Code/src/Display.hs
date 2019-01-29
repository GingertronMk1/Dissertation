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
               case (gameState fr) of PreStart      -> do let line1 = "Welcome to Functional Frogger!"
                                                              line2 = "W, A, S, and D are your movement keys"
                                                              line3 = "Space will pause the game"
                                                              line4 = "Press any key to start!"
                                                          preservingMatrix $ do translate $ Vector3 32.0 240.0 (0.0 :: Float)
                                                                                scale 0.1 0.1 (1.0 :: Float)
                                                                                drawString line1
                                                                                translate $ Vector3 0.0 (-200.0) (0.0 :: Float)
                                                                                drawString line2
                                                                                translate $ Vector3 0.0 (-200.0) (0.0 :: Float)
                                                                                drawString line3
                                                                                translate $ Vector3 0.0 (-200.0) (0.0 :: Float)
                                                                                drawString line4
                                      PlayerDead _  -> do let cause = ((\(PlayerDead s) -> s) . gameState) fr
                                                              score = "You died with " ++ show (gameScore fr) ++ " points!"
                                                              space = "Press space to play again!"
                                                          color $ Color3 1.0 1.0 (1.0 :: Float)
                                                          preservingMatrix $ do translate $ Vector3 32.0 240.0 (0.0 :: Float)
                                                                                scale 0.1 0.1 (1.0 :: Float)
                                                                                drawString cause
                                                                                translate $ Vector3 0.0 (-200.0) (0.0 :: Float)
                                                                                drawString score
                                                                                translate $ Vector3 0.0 (-200.0) (0.0 :: Float)
                                                                                drawString space
                                      LevelComplete -> do let score = show $ gameScore fr
                                                              state = "You completed the level with " ++ score ++ " points!"
                                                              space = "Press space to advance to the next level!"
                                                          color $ Color3 1.0 1.0 (1.0 :: Float)
                                                          preservingMatrix $ do translate $ Vector3 (32.0) 240.0 (0.0 :: Float)
                                                                                scale 0.1 0.1 (1.0 :: Float)
                                                                                drawString state
                                                                                translate $ Vector3 0.0 (-200.0) (0.0 :: Float)
                                                                                drawString space
                                      otherwise     -> do t <- get elapsedTime
                                                          ppStats fr
                                                          (sequence . map drawRoadLane . take 5) lanes
                                                          (sequence . map drawRiverLane . take 5 . drop 6) lanes
                                                          (preservingDraws . roadEnemies) fr
                                                          (preservingDraws . riverEnemies) fr
                                                          (preservingDraws . goals) fr
                                                          (preservingDraw . player) fr
                                                          m $~! \e -> e {frames = frames e + 1, time = t}
               swapBuffers
               where drawString s = preservingMatrix $ renderString MonoRoman s

ppStats :: Env -> IO()
ppStats e = let t = time e
                ppFrog = show $ player e
                ppMovers = (concat . intersperse "\n" . map (("  "++) . show) . roadEnemies) e
                ppFPS = if t > 0 then show $ div (frames e * 1000) t else "0"
                ppState = show $ gameState e
                ppScore = show $ gameScore e
                ppTime = show $ div t 100
             in preservingMatrix $ do color $ Color3 1.0 1.0 (1.0 :: Float)
                                      translate $ Vector3 32.0 440.0 (0.0 :: Float)
                                      preservingMatrix $ do scale 0.1 0.1 (1.0 :: Float)
                                                            renderString MonoRoman $ ppState
                                      translate $ Vector3 200.0 0.0 (0.0 :: Float)
                                      preservingMatrix $ do scale 0.1 0.1 (1.0 :: Float)
                                                            renderString MonoRoman $ ppScore

drawRiverLane :: Float -> IO()
drawRiverLane y = preservingMatrix $ do color $ Color3 0.2 0.2 (1.0 :: Float)
                                        drawLane y

drawRoadLane :: Float -> IO()
drawRoadLane y = preservingMatrix $ do color $ Color3 0.3 0.3 (0.3 :: Float)
                                       drawLane y

drawLane :: Float -> IO()
drawLane y = do translate $ Vector3 0.0 y 0.0
                scale 640.0 30.0 (1.0 :: Float)
                unitSquare
                where unitSquare = let us = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float, Float, Float)]
                                  in (renderPrimitive Quads . mapM_ makeVertex) us
                      makeVertex (x,y,z) = vertex $ Vertex3 x y z
