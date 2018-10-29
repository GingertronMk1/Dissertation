import Graphics.UI.GLUT

roadWidth :: Float
roadWidth = 0.2

roadPad :: Float
roadPad = roadWidth * 1.25

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Frogger"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  drawPlayerChar 0.0 (-0.5)
  flush

playerPoints :: Float -> Float -> Float -> [(GLfloat, GLfloat, GLfloat)]
playerPoints x y s = let n = 50 in [ (x + (s*(sin (2*pi*k/n))), y + (s*(cos (2*pi*k/n))), 0) | k <- [1..n] ]

drawPlayerChar :: Float -> Float -> IO()
drawPlayerChar x y = (renderPrimitive TriangleFan . mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z)) (playerPoints x y 0.1)
