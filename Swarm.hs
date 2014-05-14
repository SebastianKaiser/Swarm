import Graphics.UI.GLUT
import Data.IORef
import SwopenGL.Bindings
import System.Random
import SwarmLogic.GameWorld

main :: IO ()
main = do
  -- Gameworld
  gen  <- newStdGen
  let gw = createGameWorld 50 gen
  gwRef <- newIORef gw

  initOpenGl gwRef  
  prog <- createProgram 
  shdrVert <- initShader VertexShader "./SwopenGL/ModelViewShader.vert" 
  shdrFrag <- initShader FragmentShader "./Brick.frag" 
  prog <- installShaders [shdrVert, shdrFrag]
  mainLoop

initOpenGl:: IORef GameWorld -> IO ()
initOpenGl gwRef = do 
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less 
  keyboardMouseCallback $= Just (keyboardMouse)
  idleCallback $= Just (idle gwRef)
  displayCallback $= display gwRef
