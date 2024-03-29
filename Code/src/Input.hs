-- | Module: Frogger.Input
module Input where

import Graphics.Gloss.Interface.Pure.Game
import System.Random (next)
import Type

-- | The gap between lanes
laneDiff :: Float
laneDiff = lanes !! 1 - head lanes

-- | The base speed of the player while jumping
baseSpeed :: Float
baseSpeed = laneDiff / 5

-- | The boosted speed of the player while jumping
boostSpeed :: Float
boostSpeed = laneDiff / 3

-- | A routing function of sorts: it takes the event and the current Env and, based on the current gameState, determines which function to call to give the correct result.
gameInput :: Event -> Env -> Env
gameInput ev en@E {gameState = gs} = case gs of PreStart      -> inputPreStart ev en
                                                Playing       -> inputPlaying ev en
                                                Paused        -> inputPaused ev en
                                                PlayerDead _  -> inputDead ev en
                                                LevelComplete -> inputComplete ev en

-- | The function for dealing with inputs before the game has started proper.
--   Pressing any key down will start the game, other input is ignored.
inputPreStart :: Event -> Env -> Env
inputPreStart (EventKey _ Down _ _) e = assignAllSprites $ e {gameState = Playing}
inputPreStart _ e = e

-- | The function for dealing with inputs during gameplay.
--   'w', 'a', 's', and 'd' cause the player to jump up, left, down, and right respectively at the speed denoted by 'baseSpeed'.
--   Shift + the above cause the player to move at the speed denoted by 'boostSpeed'.
inputPlaying :: Event -> Env -> Env
inputPlaying (EventKey c Down _ _) e@E {player = p}
  | c == Char 'w'             = e {player = jumpY baseSpeed     p {facing = 0}}
  | c == Char 'd'             = e {player = jumpX baseSpeed     p {facing = 90}}
  | c == Char 's'             = e {player = jumpY (-baseSpeed)  p {facing = 180}}
  | c == Char 'a'             = e {player = jumpX (-baseSpeed)  p {facing = 270}}
  | c == SpecialKey KeyUp     = e {player = jumpY baseSpeed     p {facing = 0}}
  | c == SpecialKey KeyRight  = e {player = jumpX baseSpeed     p {facing = 90}}
  | c == SpecialKey KeyDown   = e {player = jumpY (-baseSpeed)  p {facing = 180}}
  | c == SpecialKey KeyLeft   = e {player = jumpX (-baseSpeed)  p {facing = 270}}
  | c == Char 'W'             = e {player = jumpY boostSpeed    p {facing = 0}}
  | c == Char 'D'             = e {player = jumpX boostSpeed    p {facing = 90}}
  | c == Char 'S'             = e {player = jumpY (-boostSpeed) p {facing = 180}}
  | c == Char 'A'             = e {player = jumpX (-boostSpeed) p {facing = 270}}
  | c == SpecialKey KeySpace  = e {gameState = Paused}
  | c == Char 'l'             = e {gameState = LevelComplete}
  | otherwise                   = e
  where setPrevs f = f {prev_dX = getdX f, prev_dY = getdY f}
        ignoringJump fn n pl = if isJumping pl then pl else fn n pl
        jumpX pl = ignoringJump (\n f -> let step = laneDiff * signum n in setdX n . setPrevs $ f {isJumpingX = True, targetX = getX f + step}) pl
        jumpY pl = ignoringJump (\n f -> let step = laneDiff * signum n in setdY n . setPrevs $ f {isJumpingY = True, targetY = getY f + step}) pl
inputPlaying _ e = e

-- | The function for dealing with inputs while the game is paused.
--   Pressing space resumes the game, all other input is ignored.
inputPaused :: Event -> Env -> Env
inputPaused (EventKey (SpecialKey KeySpace) Down _ _) e = e {gameState = Playing}
inputPaused _ e = e

-- | The function for dealing with inputs when the player has died.
--   Pressing space starts a new game with the actual screen dimensions being passed through to the new Env.
--   Other input is ignored.
inputDead :: Event -> Env -> Env
inputDead (EventKey (SpecialKey KeySpace) Down _ _) E {sWidth = sW, sHeight = sH, rGen = r, background = bg, spriteList = sprl}
  = assignAllSprites $ (startEnv sW sH (snd . next $ r)) {background = bg, spriteList = sprl}
inputDead _ e = e

-- | The function for dealing with input when a level is complete.
--   Pressing space increases the level by 1, generates new goals based on the level and the 'goalGen' function in 'Type', increases all enemy speeds by a factor of 1.2, resets the player position, and reassigns sprites.
--   All other input is ignored.
inputComplete :: Event -> Env -> Env
inputComplete (EventKey (SpecialKey KeySpace) Down _ _) e@E {level = lev}
  = let l' = lev + 1
     in assignAllSprites $ e {player = newPlayer
                             ,roadEnemies = map moddX $ roadEnemies e
                             ,riverEnemies = map moddX $ riverEnemies e
                             ,goals = goalGen l'
                             ,level = l'
                             ,gameState = Playing
                             }
  where moddX m = setdX (getdX m * 1.2) m
inputComplete _ e = e
