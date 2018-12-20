module Input (input) where

import Graphics.UI.GLUT
import Data.IORef
import Type

input :: IORef Env -> KeyboardMouseCallback
input m k ks mo p = do m' <- get m
                       if gameState m' == Playing then inputPlaying m k ks mo p
                       else if gameState m' == Paused then inputPaused m k ks mo p
                       else if gameState m' == PlayerDead then inputDead m k ks mo p
                       else return ()

inputDead :: IORef Env -> KeyboardMouseCallback
inputDead m c Down _ _
  | c == (Char ' ') = m $~! \_ -> startEnv
  | otherwise       = return ()
inputDead _ _ _ _ _ = return ()


inputPaused :: IORef Env -> KeyboardMouseCallback
inputPaused m c Down _ _
  | c == (Char ' ') = m $~! \e -> e {gameState = Playing}
  | otherwise       = return ()
inputPaused _ _ _ _ _ = return ()

inputPlaying :: IORef Env -> KeyboardMouseCallback
inputPlaying m c Down _ _
  | c == (Char 'w') || c == (Char 'W') = m $~! \e -> let p = player e
                                                     in e {player = p {y = y p + step}}
  | c == (Char 'a') || c == (Char 'A') = m $~! \e -> let p = player e
                                                     in e {player = p {x = x p - step}}
  | c == (Char 's') || c == (Char 'S') = m $~! \e -> let p = player e
                                                     in e {player = p {y = y p - step}}
  | c == (Char 'd') || c == (Char 'D') = m $~! \e -> let p = player e
                                                     in e {player = p {x = x p + step}}
  | c == (Char ' ')                    = m $~! \e -> e {gameState = Paused}
  | c == (Char '\27')                  = m $~! \e -> e {gameState = PlayerDead}
  | otherwise                          = return ()
  where step = 32
inputPlaying _ _ _ _ _ = return ()

