-- |Module: Frogger.Input
module Input where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Type

gameInput :: Event -> Env -> Env
gameInput ev en@E {gameState = gs} = case gs of PreStart      -> inputPreStart ev en
                                                Playing       -> inputPlaying ev en
                                                Paused        -> inputPaused ev en
                                                PlayerDead _  -> inputDead ev en
                                                LevelComplete -> inputComplete ev en
                                                otherwise     -> en

inputPreStart :: Event -> Env -> Env
inputPreStart (EventKey _ Down _ _) e = e {player = newPlayer, gameState = Playing}
inputPreStart _ e                     = e

inputPlaying :: Event -> Env -> Env
inputPlaying (EventKey c Down _ _) e@E {player = p}
  | c == (Char 'w') = e {player = jumpY 1    p}
  | c == (Char 'a') = e {player = jumpX (-1) p}
  | c == (Char 's') = e {player = jumpY (-1) p}
  | c == (Char 'd') = e {player = jumpX 1    p}
  | c == (Char 'W') = e {player = jumpY 4    p}
  | c == (Char 'A') = e {player = jumpX (-4) p}
  | c == (Char 'S') = e {player = jumpY (-4) p}
  | c == (Char 'D') = e {player = jumpX 4    p}
  | c == (SpecialKey KeySpace)  = e {gameState = Paused}
  | otherwise                   = e
  where step = 200
        speed = 10
        setPrevs p = p {prev_dX = getdX p, prev_dY = getdY p}
        ignoringJump f n p = if is_Jumping p then p else f n p
        jumpX p = ignoringJump (\n f -> let stepNo = step * (signum n) in setdX (speed*n) . setPrevs $ f {is_JumpingX = True, targetX = getX f + stepNo}) p
        jumpY p = ignoringJump (\n f -> let stepNo = step * (signum n) in setdY (speed*n) . setPrevs $ f {is_JumpingY = True, targetY = getY f + stepNo}) p
inputPlaying _ e    = e

inputPaused :: Event -> Env -> Env
inputPaused (EventKey (SpecialKey KeySpace) Down _ _) e = e {gameState = Playing}
inputPaused _ e                                         = e

inputDead :: Event -> Env -> Env
inputDead (EventKey (SpecialKey KeySpace) Down _ _) e@E {sWidth = sw, sHeight = sh}
  = startEnv {sWidth = sw, sHeight = sh}
inputDead _ e                                         = e

inputComplete :: Event -> Env -> Env
inputComplete (EventKey (SpecialKey KeySpace) Down _ _) e@E {level = l}
  = let l' = l + 1
     in e {player = newPlayer
          ,roadEnemies = map moddX $ roadEnemies e
          ,riverEnemies = map moddX $ riverEnemies e
          ,goals = goalGen l'
          ,level = l'
          ,gameState = Playing
          }
  where moddX m = setdX (getdX m * 1.2) m
inputComplete _ e                                                       = e
