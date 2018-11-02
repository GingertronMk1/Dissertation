module Base (draw) where
import Graphics.UI.GLUT
import Util

-- First we shall set some useful shortcut functions that return a single value
-- Here we are essentially mimicking having global static variables

width :: GLsizei    -- This is the initial width of the window we wish to draw, in pixels
width = 640

height :: GLsizei    -- This is the initial height of the window we wish to draw, in pixels
height = 480

initSize :: Size  -- Converting the above into the GLUT `Size` type
initSize = Size width height

scaleAmount :: GLfloat  -- The scale factor for the whole thing, arrived at by trial and improvement
scaleAmount = 0.000006

laneWidth :: Float  -- How wide a lane should be
laneWidth = 32.0

lanePad :: Float    -- The width of a lane + a bit either side such that multiple lanes don't resemble a large rectangle
lanePad = laneWidth * 1.25

noLanes :: Int    -- The number of each type of lane to draw
noLanes = 5

laneBlue :: Color3 Float      -- Defining the shade of blue we want the river to be
laneBlue = Color3 0.0 0.0 1.0

laneGrey :: Color3 Float      -- Defining the shade of grey we want the road to be
laneGrey = Color3 0.2 0.2 0.2

playerGreen :: Color3 Float   -- Defining the shade of green we want the player to be
playerGreen = Color3 0.0 1.0 0.0

tupleToVertex :: (a, a, a) -> Vertex3 a   -- Converting a three-tuple into a Vertex3
tupleToVertex (x,y,z) = (Vertex3 x y z)   -- This happens a lot, and it's an ugly lambda

drawPlayerChar' :: Float -> Float -> IO()                   -- Drawing a circle (50-sided polygon), this will represent the player
drawPlayerChar' x y = let playerPoints = let n = 50
                                             s = (laneWidth-1)/2 -- Want them to be slightly narrower than a lane
                                         in [(x + (s*(sin (2*pi*k/n))), y + (s*(cos (2*pi*k/n))), 0) | k <- [1..n] ]
                      in (renderPrimitive TriangleFan . mapM_ (vertex . tupleToVertex)) playerPoints

drawPlayer :: Float -> Float -> IO()      -- Applying the above but adding some colour
drawPlayer x y = preservingMatrix $ do color playerGreen
                                       drawPlayerChar' x y

drawLane :: Float -> IO()     -- A helper function to draw a lane onto the screen
drawLane y = let laneVertices = [(fromIntegral width, y+(laneWidth/2), 0.0),
                                 (0.0,                y+(laneWidth/2), 0.0),
                                 (0.0,                y-(laneWidth/2), 0.0),
                                 (fromIntegral width, y-(laneWidth/2), 0.0)]
             in (renderPrimitive Quads . mapM_ (vertex . tupleToVertex)) laneVertices

drawLanes :: (Eq t, Num t) => Float -> t -> IO ()   -- Replicating the above function some number of times
drawLanes _ 0 = return ()
drawLanes y1 n = do drawLane y1
                    drawLanes (y1+lanePad) (n-1)

drawRoadLanes :: (Eq t, Num t) => Float -> t -> IO ()   -- Using `drawLanes` to draw road lanes by applying the green colour
drawRoadLanes y1 n = preservingMatrix $ do color laneGrey
                                           drawLanes y1 n

drawRiverLanes :: (Eq t, Num t) => Float -> t -> IO ()  -- Using `drawLanes` to draw river lanes by applying the blue colour
drawRiverLanes y1 n = preservingMatrix $ do color laneBlue
                                            drawLanes y1 n

-- This function scales the viewport to a "reasonable" level.
-- Essentially the values input when drawing a shape now correspond
-- to pixels within a small frame inside the 640x480 window.
properScale :: IO()
properScale = let xScale  = ((fromIntegral height)*scaleAmount)
                  yScale  = ((fromIntegral width)*scaleAmount)
                in scale xScale yScale scaleAmount

-- This function translates the whole viewport such that x and y now start
-- at the bottom left, as it would in a drawn graph.
properTranslate :: IO()
properTranslate = let toFloat x  = 0.0-((fromIntegral x)/2.0)
                      xTranslate = toFloat width :: Float
                      yTranslate = toFloat height :: Float
                  in translate (Vector3 xTranslate yTranslate 0.0)

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  properScale
  properTranslate
  drawPlayer ((fromIntegral width)/2.0) 0
  drawRoadLanes lanePad noLanes
  drawRiverLanes (lanePad*7) noLanes
  flush

draw :: IO()
draw = do (_progName, _args) <- getArgsAndInitialize
          initialWindowSize $= initSize
          _window <- createWindow "Frogger"
          displayCallback $= display
          mainLoop
