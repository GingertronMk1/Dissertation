module Base (draw) where
import Graphics.UI.GLUT
import Util

initXSize :: GLsizei
initXSize = 640

initYSize :: GLsizei
initYSize = 480

initSize :: Size
initSize = Size initXSize initYSize

scaleAmount :: GLfloat
scaleAmount = 0.000006


laneWidth :: Float
laneWidth = 32.0

lanePad :: Float
lanePad = laneWidth * 1.25

noLanes :: Int
noLanes = 5

laneYs :: Int -> [Float]
laneYs n = let lanes = take n [1..]
           in map (lanePad*) lanes

reshape :: ReshapeCallback
reshape size = do viewport $= (Position 0 0, size)
                  postRedisplay Nothing

tupleToVertex :: (a, a, a) -> Vertex3 a
tupleToVertex (x,y,z) = (Vertex3 x y z)

playerPoints :: Float -> Float -> Float -> [(GLfloat, GLfloat, GLfloat)]
playerPoints x y s = let n = 50 in [(x + (s*(sin (2*pi*k/n))), y + (s*(cos (2*pi*k/n))), 0) | k <- [1..n] ]

drawPlayerChar :: Float -> Float -> IO()
drawPlayerChar x y = (renderPrimitive TriangleFan . mapM_ (vertex . tupleToVertex)) (playerPoints x y (laneWidth/2))

drawLane :: Float -> IO()
drawLane y = let laneVertices =[(320, y+(laneWidth/2), 0),(-320, y+(laneWidth/2), 0),(-320,y-(laneWidth/2),0),(320,y-(laneWidth/2),0)]
             in (renderPrimitive Quads . mapM_ (vertex . tupleToVertex)) laneVertices

fullScreenSquare = let xRad     = (fromIntegral initXSize)/2 :: Float
                       yRad     = (fromIntegral initYSize)/2 :: Float
                       vertices = [(xRad, yRad, 0.0),((-xRad), yRad, 0.0),((-xRad), (-yRad), 0.0),(xRad, (-yRad), 0.0)]
                   in (renderPrimitive Quads . mapM (vertex . tupleToVertex)) vertices

-- This function scales the viewport to a "reasonable" level.
-- Essentially the values input when drawing a shape now correspond
-- to pixels within a small frame inside the 640x480 window.
properScale :: IO()
properScale = let xScale  = ((fromIntegral initYSize)*scaleAmount)
                  yScale  = ((fromIntegral initXSize)*scaleAmount)
                in scale xScale yScale scaleAmount

-- This function translates the whole viewport such that x and y now start
-- at the bottom left, as it would in a drawn graph.
properTranslate :: IO()
properTranslate = let toFloat x  = 0.0-((fromIntegral x)/2.0)
                      xTranslate = toFloat initXSize :: Float
                      yTranslate = toFloat initYSize :: Float
                  in translate (Vector3 xTranslate yTranslate 0.0)

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  properScale
  properTranslate
  (sequence . map (\(x,y) -> drawPlayerChar x y)) [(x,y) | x <- [0.0,32.0..640.0], y <- [0.0,32.0..480.0]]
  flush

draw :: IO()
draw = do (_progName, _args) <- getArgsAndInitialize
          initialWindowSize $= initSize
          _window <- createWindow "Frogger"
          displayCallback $= display
          mainLoop
