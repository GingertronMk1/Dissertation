module Display (display) where

import Graphics.UI.GLUT

display :: DisplayCallback
display = do clear [ColorBuffer]
             translate $ Vector3 (-1.0) (-1.0) (0.0 :: Float)
             scale (1.0/32.0) (1.0/24.0) (0.0 :: Float)
             color $ Color3 0.0 1.0 (0.0 :: Float)
             let p = [(1,0,0),(1,1,0),(0,1,0),(0,0,0)] :: [(Float,Float,Float)]
             renderPrimitive Quads $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) p
             flush
