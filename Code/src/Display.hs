module Display (display) where

import Graphics.UI.GLUT
import Data.IORef
import Type

display :: IORef Mover -> DisplayCallback
display m = do clear [ColorBuffer]
               loadIdentity
               fr <- get m
               putStrLn $ show fr
               translate $ Vector3 (-1.0) (-1.0) (0.0 :: Float)
               scale (1.0/320.0) (1.0/240.0) (0.0 :: Float)
               preservingMatrix $ do color $ Color3 0.0 1.0 (0.0 :: Float)
                                     let p = [(x fr + s fr,y fr,0),
                                               (x fr + s fr,y fr + s fr,0),
                                               (x fr,y fr + s fr,0),
                                               (x fr,y fr,0)] :: [(Float,Float,Float)]
                                     renderPrimitive Quads $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) p
               swapBuffers
