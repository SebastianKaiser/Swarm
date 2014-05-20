import Graphics.UI.GLUT
import Data.IORef
import Data.Array.Storable
import SwopenGL.Bindings
import System.Random
import SwarmLogic.GameWorld
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.GLUtil.BufferObjects
import Graphics.GLUtil.VertexArrayObjects
import Linear.Vector
import Linear.V4

main :: IO ()
main = do
  -- Gameworld
  gen  <- newStdGen
  let gw = createGameWorld 50 gen
  gwRef <- newIORef gw

  initOpenGl gwRef  

  shdrVert <- initShader VertexShader "./resources/ModelViewShader.vert"
  shdrFrag <- initShader FragmentShader "./resources/FragShader.frag" 
  prog <- installShaders [shdrVert, shdrFrag]

  let vertArray = [ 0, 0, -50, 
                    1, 0, -50,
                    0, 1, -50]
       
  let vertIndexArray = [1, 2, 3]

  vbo <- vboOfList ElementArrayBuffer (length vertIndexArray) vertIndexArray StaticDraw
  vio <- vboOfList ArrayBuffer (length vertArray) vertArray StaticDraw

  let vertDesc = VertexArrayDescriptor 3 Float 0 offset0 
  arrayPointer VertexArray $= vertDesc

  clientState VertexArray $= Enabled
  clientState IndexArray  $= Enabled

  mainLoop

vboOfList :: BufferTarget -> Int -> [GLfloat] -> BufferUsage -> IO BufferObject
vboOfList buffTarg size elems buffUse = do
    let ptrsize = toEnum $ size * 4                -- toEnum macht aus Int GLsizei
    (array:_) <- genObjectNames 1                  -- array Objectname
    bindBuffer buffTarg  $= Just array             -- macht den Buffer bei OpenGL bekannt
    arr <- newListArray (0, size - 1) elems        -- erzeugt ein mutable array aus elems
    withStorableArray arr $ \ptr -> bufferData buffTarg $= (ptrsize, ptr,  buffUse)
    return array

initOpenGl:: IORef GameWorld -> IO ()
initOpenGl gwRef = do 
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Hello World"
  depthFunc $= Just Less 
  keyboardMouseCallback $= Just (keyboardMouse)
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle gwRef)
  displayCallback $= display gwRef
