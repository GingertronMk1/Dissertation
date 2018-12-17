module Type (Mover(..)) where

data Mover = Frogger {x :: Float,
                      y :: Float,
                      s :: Float
                     } deriving Show
