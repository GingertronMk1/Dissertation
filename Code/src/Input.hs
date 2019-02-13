-- |Module: Frogger.Input
module Input where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Type

-- | The base speed of the player while jumping
baseSpeed :: Float
baseSpeed = 10

-- | The boosted speed of the player while jumping
boostSpeed :: Float
boostSpeed = 40

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
inputPreStart (EventKey _ Down _ _) e = e {player = newPlayer, gameState = Playing}
inputPreStart _ e = e

-- | The function for dealing with inputs during gameplay.
--   'w', 'a', 's', and 'd' cause the player to jump up, left, down, and right respectively at the speed denoted by 'baseSpeed'.
--   Shift + the above cause the player to move at the speed denoted by 'boostSpeed'
inputPlaying :: Event -> Env -> Env
inputPlaying (EventKey c Down _ _) e@E {player = p}
  | c == (Char 'w') = e {player = jumpY baseSpeed     p {facing = 0}}
  | c == (Char 'd') = e {player = jumpX baseSpeed     p {facing = 90}}
  | c == (Char 's') = e {player = jumpY (-baseSpeed)  p {facing = 180}}
  | c == (Char 'a') = e {player = jumpX (-baseSpeed)  p {facing = 270}}
  | c == (Char 'W') = e {player = jumpY boostSpeed    p {facing = 0}}
  | c == (Char 'D') = e {player = jumpX boostSpeed    p {facing = 90}}
  | c == (Char 'S') = e {player = jumpY (-boostSpeed) p {facing = 180}}
  | c == (Char 'A') = e {player = jumpX (-boostSpeed) p {facing = 270}}
  | c == (SpecialKey KeySpace)  = e {gameState = Paused}
  | otherwise                   = e
  where setPrevs p = p {prev_dX = getdX p, prev_dY = getdY p}
        ignoringJump f n p = if is_Jumping p then p else f n p
        jumpX p = ignoringJump (\n f -> let step = 200 * (signum n) in setdX n . setPrevs $ f {is_JumpingX = True, targetX = getX f + step}) p
        jumpY p = ignoringJump (\n f -> let step = 200 * (signum n) in setdY n . setPrevs $ f {is_JumpingY = True, targetY = getY f + step}) p
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
inputDead (EventKey (SpecialKey KeySpace) Down _ _) e@E {sWidth = sw, sHeight = sh}
  = startEnv {sWidth = sw, sHeight = sh}
inputDead _ e = e

-- | The function for dealing with input when a level is complete.
--   Pressing space increases the level by 1, generates new goals based on the level and the 'goalGen' function in 'Type', increases all enemy speeds by a factor of 1.2, and resets the player position.
--   All other input is ignored.
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
inputComplete _ e = e
