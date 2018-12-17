module Display (display) where

import Graphics.UI.GLUT

display :: DisplayCallback
display = do clear [ColorBuffer]
             flush
