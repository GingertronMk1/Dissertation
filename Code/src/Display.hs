module Display (display) where

import Graphics.UI.GLUT

display = do clear [ColorBuffer]
             translate $ Vector3 (-1.0) (-1.0) (0.0 :: Float)
             scale (1.0/320.0) (1.0/240.0) (0.0 :: Float)
             preservingMatrix $ do color $ Color3 0.0 1.0 (0.0 :: Float)
                                   let p = [(10, 0,  0),
                                            (10, 10, 0),
                                            (0 , 10, 0),
                                            (0 , 0,  0)] :: [(Float,Float,Float)]
                                   renderPrimitive Quads $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) p
             flush
