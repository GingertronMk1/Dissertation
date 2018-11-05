module Base (draw) where
import Graphics.UI.GLUT
import Util

-- First we shall set some useful shortcut functions that return a single value
-- Here we are essentially mimicking having global static variables

width :: GLsizei    -- This is the initial width of the window we wish to draw, in pixels
width = 640

height :: GLsizei    -- This is the initial height of the window we wish to draw, in pixels
height = 480

noLanes :: Int    -- The number of each type of lane to draw
noLanes = 5

initSize :: Size  -- Converting the above into the GLUT `Size` type
initSize = Size width height

gridSize :: Float    -- The width of a lane + a bit either side such that multiple lanes don't resemble a large rectangle
gridSize = (fromIntegral height)/(fromIntegral ((2*noLanes) + 3)) -- Need to fit 2n+3 lanes in total on the screen

laneWidth :: Float  -- How wide a lane should be
laneWidth = gridSize/1.25

laneBlue :: Color3 Float      -- Defining the shade of blue we want the river to be
laneBlue = Color3 0.0 0.0 1.0

laneGrey :: Color3 Float      -- Defining the shade of grey we want the road to be
laneGrey = let greyVal = 0.4 in Color3 greyVal greyVal greyVal

playerGreen :: Color3 Float   -- Defining the shade of green we want the player to be
playerGreen = Color3 0.0 1.0 0.0

tupleToVertex :: VertexComponent a => (a, a, a) -> IO ()  -- Converting a three-tuple into a Vertex3
tupleToVertex (x,y,z) = vertex (Vertex3 x y z)             -- This happens a lot, and it's an ugly lambda function

drawPlayerChar' :: Float -> Float -> IO()                   -- Drawing a circle (50-sided polygon), this will represent the player
drawPlayerChar' x y = let playerPoints = let n = 50
                                             s = (laneWidth-1)/2 -- Want them to be slightly narrower than a lane
                                         in [(x + (s*(sin (2*pi*k/n))), y + (s*(cos (2*pi*k/n))), 0) | k <- [1..n] ]
                      in (renderPrimitive TriangleFan . mapM_ tupleToVertex) playerPoints

drawPlayer :: Float -> Float -> IO()      -- Applying the above but adding some colour
drawPlayer x y = preservingMatrix $ do color playerGreen
                                       drawPlayerChar' x y

drawLane :: Float -> IO()     -- A helper function to draw a lane onto the screen
drawLane y = let laneVertices = [(fromIntegral width, y+laneWidth,  0.0),
                                 (0.0,                y+laneWidth,  0.0),
                                 (0.0,                y,            0.0),
                                 (fromIntegral width, y,            0.0)]
             in (renderPrimitive Quads . mapM_ tupleToVertex) laneVertices

drawLanes :: (Eq t, Num t) => Float -> t -> IO ()   -- Replicating the above function some number of times
drawLanes _ 0 = return ()
drawLanes y1 n = do drawLane y1
                    drawLanes (y1+gridSize) (n-1)

drawAllLanes = preservingMatrix $ do color laneGrey
                                     drawLanes gridSize noLanes
                                     color laneBlue
                                     drawLanes (gridSize*(fromIntegral noLanes + 2)) noLanes

-- This function translates the whole viewport such that x and y now start
-- at the bottom left, as it would in a drawn graph.
properTranslate :: IO()
properTranslate = translate (Vector3 (-1.0) (-1.0) (0.0 :: Float))

-- This function scales the viewport to a "reasonable" level.
-- Essentially the values input when drawing a shape now correspond
-- to pixels within a small frame inside the 640x480 window.
properScale :: IO()
properScale = let xScale = (2.0/(fromIntegral width))
                  yScale = (2.0/(fromIntegral height))
              in scale xScale yScale (0.0 :: Float)

display :: DisplayCallback
display = do clear [ColorBuffer]
             properTranslate
             properScale
             drawAllLanes
             drawPlayer ((fromIntegral width)/2.0) (gridSize*0.5)
             flush

draw :: IO()
draw = do (_progName, _args) <- getArgsAndInitialize
          initialWindowSize $= initSize
          _window <- createWindow "Frogger"
          displayCallback $= display
          mainLoop
