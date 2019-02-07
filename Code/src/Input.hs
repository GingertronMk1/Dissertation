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

inputPreStart :: Event -> Env -> Env
inputPreStart (EventKey (SpecialKey KeySpace) Down _ _) e = e {gameState = Playing}
inputPreStart _ e                                         = e

inputPlaying :: Event -> Env -> Env
inputPlaying (EventKey (Char 'w') Down _ _) e             = e {player = playerJumpY 1    (player e)}
inputPlaying (EventKey (Char 'a') Down _ _) e             = e {player = playerJumpX (-1) (player e)}
inputPlaying (EventKey (Char 's') Down _ _) e             = e {player = playerJumpY (-1) (player e)}
inputPlaying (EventKey (Char 'd') Down _ _) e             = e {player = playerJumpX 1    (player e)}
inputPlaying (EventKey (Char 'W') Down _ _) e             = e {player = playerJumpY 4    (player e)}
inputPlaying (EventKey (Char 'A') Down _ _) e             = e {player = playerJumpX (-4) (player e)}
inputPlaying (EventKey (Char 'S') Down _ _) e             = e {player = playerJumpY (-4) (player e)}
inputPlaying (EventKey (Char 'D') Down _ _) e             = e {player = playerJumpX 4    (player e)}
inputPlaying (EventKey (SpecialKey KeySpace) Down _ _) e  = e {gameState = Paused}
inputPlaying _ e                                          = e

inputPaused :: Event -> Env -> Env
inputPaused (EventKey (SpecialKey KeySpace) Down _ _) e = e {gameState = Playing}
inputPaused _ e                                         = e

inputDead :: Event -> Env -> Env
inputDead (EventKey (SpecialKey KeySpace) Down _ _) e = startEnv 1
inputDead _ e                                         = e

inputComplete :: Event -> Env -> Env
inputComplete (EventKey (SpecialKey KeySpace) Down _ _) e@E {level = l} = (startEnv (l+1)) {frames = frames e
                                                                                           ,time = time e
                                                                                           ,gameState = Playing
                                                                                           ,gameScore = gameScore e
                                                                                           }
inputComplete _ e                                                       = e

step :: Float
step = 32

speed :: Float
speed = 2

setPrevs :: Frogger -> Frogger
setPrevs p = p {prev_dX = getdX p, prev_dY = getdY p}

ignoringJump :: (Float -> Frogger -> Frogger) -> Float -> Frogger -> Frogger
ignoringJump f n p = if is_Jumping p then p else f n p

playerJumpX :: Float -> Frogger -> Frogger
playerJumpX p = ignoringJump (\n f -> let stepNo = step * (signum n) in setdX (speed*n) . setPrevs $ f {is_JumpingX = True, targetX = getX f + stepNo}) p

playerJumpY :: Float -> Frogger -> Frogger
playerJumpY p = ignoringJump (\n f -> let stepNo = step * (signum n) in setdY (speed*n) . setPrevs $ f {is_JumpingY = True, targetY = getY f + stepNo}) p
