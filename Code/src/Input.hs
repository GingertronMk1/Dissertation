module Input (input) where

import Graphics.UI.GLUT
import Data.IORef
import Type

input :: IORef Mover -> KeyboardMouseCallback
input m c Down _ _ | c == (Char 'w') || c == (Char 'W') = m $~! \f -> f {y = y f + step}
                   | c == (Char 'a') || c == (Char 'A') = m $~! \f -> f {x = x f - step}
                   | c == (Char 's') || c == (Char 'S') = m $~! \f -> f {y = y f - step}
                   | c == (Char 'd') || c == (Char 'D') = m $~! \f -> f {x = x f + step}
                   | otherwise                          = return ()
                   where step = 5
input _ _ _ _ _ = return ()
