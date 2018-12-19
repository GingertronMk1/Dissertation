module Display (display) where

import Graphics.UI.GLUT
import Data.IORef
import Type

display :: IORef Env -> DisplayCallback
display m = do clear [ColorBuffer]
               loadIdentity
               fr <- get m
               translate $ Vector3 (-1.0) (-1.0) (0.0 :: Float)
               scale (1.0/320.0) (1.0/240.0) (0.0 :: Float)
               let lanes = [0.0,32.0..] :: [Float]
               (sequence . map drawRoadLane . take 5 . drop 1) lanes
               (sequence . map drawRiverLane . take 5 . drop 7) lanes
               (sequence . map drawMover . enemies) fr
               (drawMover . player) fr
               swapBuffers

drawMover :: Mover -> IO()
drawMover m = preservingMatrix $ drawMover' m

drawMover' :: Mover -> IO()
drawMover' Frogger {x = fx, y = fy, s = fs} = do color $ Color3 0.0 1.0 (0.0 :: Float)
                                                 translate $ Vector3 fx fy 0.0
                                                 scale fs fs 1.0
                                                 unitSquare
drawMover' Car {x = cx, y = cy, l = cl, w = cw, v = cv} = do color $ Color3 1.0 0.0 (0.0 :: Float)
                                                             translate $ Vector3 cx cy 0.0
                                                             scale (cl*(signum cv)) cw 1.0
                                                             unitSquare
                                                             color $ Color3 0.3 0.3 (1.0 :: Float)
                                                             translate $ Vector3 0.8 0.0 (0.0 :: Float)
                                                             scale 0.1 1.0 (1.0 :: Float)
                                                             unitSquare

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

unitSquare :: IO()
unitSquare = let us = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float, Float, Float)]
             in (renderPrimitive Quads . mapM_ makeVertex) us

makeVertex :: (Float, Float, Float) -> IO()
makeVertex (x,y,z) = vertex $ Vertex3 x y z
