import Graphics.UI.GLUT
import Data.IORef
import SwopenGL.Bindings
import System.Random
import SwarmLogic.GameWorld
 
main :: IO ()
main = do
  -- Gameworld
  gen  <- newStdGen
  let gw = createGameWorld 10 gen
  gwRef <- newIORef gw
  -- OpenGL
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less 
  angle <- newIORef 0
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  idleCallback $= Just (idle gwRef)
  displayCallback $= display angle pos gwRef
  mainLoop
