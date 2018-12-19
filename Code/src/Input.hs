module Input (input) where

import Graphics.UI.GLUT
import Data.IORef
import Type

input :: IORef Env -> KeyboardMouseCallback
input m c Down _ _
  | c == (Char 'w') || c == (Char 'W') = m $~! \e -> let p = player e
                                                     in e {player = p {y = y p + step}}
  | c == (Char 'a') || c == (Char 'A') = m $~! \e -> let p = player e
                                                     in e {player = p {x = x p - step}}
  | c == (Char 's') || c == (Char 'S') = m $~! \e -> let p = player e
                                                     in e {player = p {y = y p - step}}
  | c == (Char 'd') || c == (Char 'D') = m $~! \e -> let p = player e
                                                     in e {player = p {x = x p + step}}
  | otherwise                          = return ()
  where step = 32
input _ _ _ _ _ = return ()
