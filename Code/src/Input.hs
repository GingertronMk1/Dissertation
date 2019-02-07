-- |Module: Frogger.Input
module Input where

import Graphics.UI.GLUT
import Data.IORef
import Type

-- |Essentially a routing function, which calls the requisite function depending on what the current 'GameState' is
input :: IORef Env -> KeyboardMouseCallback
input m k ks mo p = get m >>= \m' -> case gameState m' of PreStart      -> inputPreStart m k ks mo p
                                                          Playing       -> inputPlaying m k ks mo p
                                                          Paused        -> inputPaused m k ks mo p
                                                          PlayerDead _  -> inputDead m k ks mo p
                                                          LevelComplete -> inputComplete m k ks mo p

-- |The input handler for when the level is complete.
--  Pressing space will advance to the next level, any other input is ignored.
inputComplete :: IORef Env -> KeyboardMouseCallback
inputComplete m c Down _ _
  | c == (Char ' ')   = m $~! \e -> let nextLevel = level e + 1
                                     in (startEnv nextLevel) { frames = frames e
                                                             , time = time e
                                                             , gameState = Playing
                                                             , gameScore = gameScore e
                                                             , level = nextLevel
                                                             }
  | c == (Char '\27') = m $~! \e -> e {gameState = PlayerDead "You quit"}
  | otherwise         = return ()
inputComplete _ _ _ _ _ = return ()

-- |The input handler for when the player is dead.
--  Pressing space will restart the game, any other input is ignored.
inputDead :: IORef Env -> KeyboardMouseCallback
inputDead m c Down _ _
  | c == (Char ' ') = m $~! \_ -> startEnv 1
  | otherwise       = return ()
inputDead _ _ _ _ _ = return ()

-- |The input handler for when the game has yet to start.
--  Any input starts the game.
inputPreStart :: IORef Env -> KeyboardMouseCallback
inputPreStart m _ _ _ _ = m $~! \e -> e {gameState = Playing}

-- |The input handler for when the level is paused.
--  Pressing space will unpause, any other input is ignored.
inputPaused :: IORef Env -> KeyboardMouseCallback
inputPaused m c Down _ _
  | c == (Char ' ')   = m $~! \e -> e {gameState = Playing}
  | c == (Char '\27') = m $~! \e -> e {gameState = PlayerDead "You quit"}
  | otherwise         = return ()
inputPaused _ _ _ _ _ = return ()

-- |The input handler for when the game is in progress.
--  'w', 'a', 's', and 'd' cause the player to jump up, left, down, and right respectively.
--  Shift + the above majes you jump faster
--  Pressing space will pause the level, and pressing Esc will quit the game.
inputPlaying :: IORef Env -> KeyboardMouseCallback
inputPlaying m c Down _ _
  | c == (Char 'w')   = m $~! \e -> e {player = playerJumpY (player e) 1}
  | c == (Char 'a')   = m $~! \e -> e {player = playerJumpX (player e) (-1)}
  | c == (Char 's')   = m $~! \e -> e {player = playerJumpY (player e) (-1)}
  | c == (Char 'd')   = m $~! \e -> e {player = playerJumpX (player e) 1}
  | c == (Char 'W')   = m $~! \e -> e {player = playerJumpY (player e) 2}
  | c == (Char 'A')   = m $~! \e -> e {player = playerJumpX (player e) (-2)}
  | c == (Char 'S')   = m $~! \e -> e {player = playerJumpY (player e) (-2)}
  | c == (Char 'D')   = m $~! \e -> e {player = playerJumpX (player e) 2}
  | c == (Char ' ')   = m $~! \e -> e {gameState = Paused}
  | c == (Char '\27') = m $~! \e -> e {gameState = PlayerDead "You quit"}
  | otherwise         = return ()
  where step = 32
        speed = 1
        setPrevs p = p {prev_dX = getdX p, prev_dY = getdY p}
        ignoringJump p f n = if is_Jumping p then p else f n p
        playerJumpX p = ignoringJump p (\n f -> let stepNo = step * (signum n) in setdX (speed*n) . setPrevs $ f {is_JumpingX = True, targetX = getX f + stepNo})
        playerJumpY p = ignoringJump p (\n f -> let stepNo = step * (signum n) in setdY (speed*n) . setPrevs $ f {is_JumpingY = True, targetY = getY f + stepNo})
inputPlaying _ _ _ _ _ = return ()
