module Input (input) where

import Graphics.UI.GLUT
import Data.IORef
import Type

input :: IORef Env -> KeyboardMouseCallback
input m k ks mo p = get m >>= \m' -> case gameState m' of PreStart      -> inputPreStart m k ks mo p
                                                          Playing       -> inputPlaying m k ks mo p
                                                          Paused        -> inputPaused m k ks mo p
                                                          PlayerDead _  -> inputDead m k ks mo p
                                                          LevelComplete -> inputComplete m k ks mo p

inputComplete :: IORef Env -> KeyboardMouseCallback
inputComplete m c Down _ _
  | c == (Char ' ') = m $~! \e -> let nextLevel = level e + 1
                                   in (startEnv nextLevel) { frames = frames e
                                                           , time = time e
                                                           , gameState = Playing
                                                           , gameScore = gameScore e
                                                           , level = nextLevel
                                                           }
  | otherwise       = return ()
inputComplete _ _ _ _ _ = return ()

inputDead :: IORef Env -> KeyboardMouseCallback
inputDead m c Down _ _
  | c == (Char ' ') = m $~! \_ -> startEnv 1
  | otherwise       = return ()
inputDead _ _ _ _ _ = return ()

inputPreStart :: IORef Env -> KeyboardMouseCallback
inputPreStart m _ _ _ _ = m $~! \e -> e {gameState = Playing}

inputPaused :: IORef Env -> KeyboardMouseCallback
inputPaused m c Down _ _
  | c == (Char ' ') = m $~! \e -> e {gameState = Playing}
  | otherwise       = return ()
inputPaused _ _ _ _ _ = return ()

inputPlaying :: IORef Env -> KeyboardMouseCallback
inputPlaying m c Down _ _
  | c == (Char 'w') || c == (Char 'W') = m $~! \e -> let p = player e
                                                     in e {player = p {f_Y = f_Y p + step}}
  | c == (Char 'a') || c == (Char 'A') = m $~! \e -> let p = player e
                                                     in e {player = p {f_X = f_X p - step}}
  | c == (Char 's') || c == (Char 'S') = m $~! \e -> let p = player e
                                                     in e {player = p {f_Y = f_Y p - step}}
  | c == (Char 'd') || c == (Char 'D') = m $~! \e -> let p = player e
                                                     in e {player = p {f_X = f_X p + step}}
  | c == (Char ' ')                    = m $~! \e -> e {gameState = Paused}
  | c == (Char '\27')                  = m $~! \e -> e {gameState = PlayerDead "You quit"}
  | otherwise                          = return ()
  where step = 32
inputPlaying _ _ _ _ _ = return ()
