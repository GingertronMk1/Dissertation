module Util where

flatten :: [[a]] -> [a]
flatten ass = [a | as <- ass, a <- as]

squareCoords :: Int -> Int -> [(Int, Int, Int)]
squareCoords s m = [(s+m,s+m,0),(m,s+m,0),(m,m,0),(s+m,m,0)]

multiSquares s m 0 = []
multiSquares s m n = (squareCoords s m):(multiSquares s s (n-1))

