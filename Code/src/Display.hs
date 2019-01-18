module Display (display) where

import Graphics.UI.GLUT
import Data.IORef
import Data.List (intersperse)
import Type

display :: IORef Env -> DisplayCallback
display m = do clear [ColorBuffer]
               loadIdentity
               translate $ Vector3 (-1.0) (-1.0) (0.0 :: Float)
               scale (1.0/320.0) (1.0/240.0) (0.0 :: Float)
               fr <- get m
               t <- get elapsedTime
               ppStats fr
               let lanes = [0.0,32.0..] :: [Float]
               (sequence . map drawRoadLane . take 5 . drop 1) lanes
               (sequence . map drawRiverLane . take 5 . drop 7) lanes
               (sequence . map drawMover . enemies) fr
               (sequence . map drawMover . goals) fr
               (drawMover . player) fr
               m $~! \e -> e {frames = frames e + 1, time = t}
               swapBuffers

ppStats :: Env -> IO()
ppStats e = let ppFrog = show $ player e
                ppMovers = (concat . intersperse "\n" . map (("  "++) . show) . enemies) e
                ppFPS = if (time e) > 0 then show $ div (frames e * 1000) (time e) else "0"
                ppState = show $ gameState e
                ppScore = show $ gameScore e
             in preservingMatrix $ do color $ Color3 1.0 1.0 (1.0 :: Float)
                                      translate $ Vector3 32.0 440.0 (0.0 :: Float)
                                      preservingMatrix $ do scale 0.1 0.1 (1.0 :: Float)
                                                            renderString MonoRoman $ ppState
                                      translate $ Vector3 200.0 0.0 (0.0 :: Float)
                                      preservingMatrix $ do scale 0.1 0.1 (1.0 :: Float)
                                                            renderString MonoRoman $ ppScore
                                      putStr "\ESC[2J"
                                      (putStrLn . concat . intersperse "\n") [ppFrog,"Movers",ppMovers,ppFPS,ppState]

drawMover :: Mover -> IO()
drawMover m = preservingMatrix $ drawMover' m

drawMover' :: Mover -> IO()
drawMover' Frogger {x = fx, y = fy, s = fs} = do color $ Color3 0.0 1.0 (0.0 :: Float)
                                                 translate $ Vector3 fx fy 0.0
                                                 scale fs fs 1.0
                                                 unitSquare
drawMover' Car {x = cx, y = cy, l = cl, w = cw, v = cv} = do color $ Color3 1.0 0.0 (0.0 :: Float)
                                                             translate $ Vector3 cx cy 0.0
                                                             scale cl cw 1.0
                                                             unitSquare
                                                             color $ Color3 0.3 0.3 (1.0 :: Float)
                                                             translate $ Vector3 0.8 0.0 (0.0 :: Float)
                                                             scale 0.1 1.0 (1.0 :: Float)
                                                             unitSquare
drawMover' Croc {x = cx, y = cy, l = cl, w = cw, v = cv} = do color $ Color3 0.0 0.5 (0.0 :: Float)
                                                              translate $ Vector3 cx cy 0.0
                                                              scale cl cw 1.0
                                                              unitSquare
                                                              color $ Color3 1.0 1.0 (1.0 :: Float)
                                                              preservingMatrix $ do translate $ Vector3 0.8 (0.1) (0.0 :: Float)
                                                                                    scale 0.1 0.2 (1.0 :: Float)
                                                                                    unitSquare
                                                              preservingMatrix $ do translate $ Vector3 0.8 (0.8) (0.0 :: Float)
                                                                                    scale 0.1 0.2 (1.0 :: Float)
                                                                                    unitSquare
drawMover' Turtles {x = tx, y = ty, l = tl, w = tw, v = tv} = do translate $ Vector3 tx ty 0.0
                                                                 scale (tw * signum tv) tw 1.0
                                                                 drawTurtles
drawMover' Log {x = lx, y = ly, l = ll, w = lw, v = lv} = do color $ Color3 0.6 0.3 (0.2 :: Float)
                                                             translate $ Vector3 lx ly 0.0
                                                             scale ll lw 1.0
                                                             unitSquare
drawMover' Goal {x = gx, y = gy, s = gs} = do color $ Color3 0.8 0.7 (0.2 :: Float)
                                              translate $ Vector3 gx gy 0.0
                                              scale gs gs 1.0
                                              unitSquare


drawTurtle = do preservingMatrix $ do translate $ Vector3 0.1 0.1 (0.0 :: Float)
                                      scale 0.8 0.8 (1.0 :: Float)
                                      color $ Color3 0.0 1.0 (0.0 :: Float)
                                      unitSquare
                color $ Color3 1.0 0.6 (0.0 :: Float)
                unitCircle

drawTurtles = do drawTurtle
                 translate $ Vector3 1.0 0.0 (0.0 :: Float)
                 drawTurtle
                 translate $ Vector3 1.0 0.0 (0.0 :: Float)
                 drawTurtle

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

unitCircle :: IO()
unitCircle = let n = 50.0
                 points = [(0.5*(sin (2*pi*k/n)+1.0), 0.5*(cos (2*pi*k/n)+1.0), 0) | k <- [1..n]]
              in (renderPrimitive Polygon . mapM_ makeVertex) points

unitSquare :: IO()
unitSquare = let us = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float, Float, Float)]
             in (renderPrimitive Quads . mapM_ makeVertex) us

makeVertex :: (Float, Float, Float) -> IO()
makeVertex (x,y,z) = vertex $ Vertex3 x y z
