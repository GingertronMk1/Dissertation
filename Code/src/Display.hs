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
               (drawMover . player) fr
               (sequence . map drawMover . enemies) fr
               swapBuffers

drawMover :: Mover -> IO()
drawMover m = preservingMatrix $ drawMover' m

drawMover' :: Mover -> IO()
drawMover' Frogger {x = fx, y = fy, s = fs} = do color $ Color3 0.0 1.0 (0.0 :: Float)
                                                 translate $ Vector3 fx fy 0.0
                                                 scale fs fs 1.0
                                                 unitSquare
drawMover' Car {x = cx, y = cy, l = cl, w = cw} = do color $ Color3 1.0 0.0 (0.0 :: Float)
                                                     translate $ Vector3 cx cy 0.0
                                                     scale cl cw 1.0
                                                     unitSquare

unitSquare :: IO()
unitSquare = let us = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float, Float, Float)]
             in (renderPrimitive Quads . mapM_ makeVertex) us

makeVertex :: (Float, Float, Float) -> IO()
makeVertex (x,y,z) = vertex $ Vertex3 x y z
