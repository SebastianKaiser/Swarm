module SwopenGL.Bindings (
idle
, display
, reshape
, keyboardMouse
, initShader
, installShaders
, setProjMatrix
, vboOfList
, initOpenGl
, initBoidGraphics
) where
 
import SwopenGL.Display
import Graphics.Rendering.OpenGL 
import System.Exit ( exitWith, ExitCode(..) )
import qualified Data.ByteString as BS
import Control.Monad (unless)
import Data.Maybe
import Data.IORef
import Data.Array.Storable
import Foreign.Ptr
import Constants
import Graphics.UI.GLUT 
import Graphics.GLUtil 
import Graphics.Rendering.OpenGL (BufferObject)
import Graphics.GLUtil.Camera3D hiding (orientation)
import SwarmLogic.GameWorld
 
reshape :: ReshapeCallback
reshape (Size w h) = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  setProjMatrix (Size w h)

setProjMatrix :: Size -> IO ()
setProjMatrix (Size w h) = do
  prog <- get currentProgram
  projLoc <- get $ uniformLocation (fromJust prog) "Projection"
  let ratio = (fromIntegral w)/(fromIntegral h)::GLfloat
  asUniform (projectionMatrix (pi/4) ratio 0.1 (-20)) projLoc

keyboardMouse :: KeyboardMouseCallback
keyboardMouse (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _ _ _ = return ()

initShader:: ShaderType -> FilePath -> IO Shader
initShader shdrType path = do
  src <- BS.readFile path
  shdr <- createShader shdrType
  shaderSourceBS shdr $= src
  compileShader shdr
  reportErrors
  ok <- get $ compileStatus shdr
  infoLog <- get (shaderInfoLog shdr)
  mapM_ putStrLn ["Shader info log for '" ++ path ++ "':", infoLog, ""]
  unless ok $ do
    deleteObjectNames [shdr]
    ioError (userError "shader compilation failed")
  return shdr

installShaders :: [Shader] -> IO Program
installShaders shaders = do
   prog <- createProgram
   attachedShaders prog $= shaders
   linkProgram prog
   reportErrors
   ok <- get (linkStatus prog)
   infoLog <- get (programInfoLog prog)
   mapM_ putStrLn ["Program info log:", infoLog, ""]
   unless ok $ do
      deleteObjectNames [prog]
      ioError (userError "linking failed")
   currentProgram $= Just prog
   return prog

vboOfList :: BufferTarget -> Int -> [GLfloat] -> BufferUsage -> IO BufferObject
vboOfList buffTarg size elems buffUse = do
  let ptrsize = toEnum $ size * 4                -- toEnum macht aus Int GLsizei
  (array:_) <- genObjectNames 1                  -- array Objectname
  bindBuffer buffTarg  $= Just array             -- macht den Buffer bei OpenGL bekannt
  arr <- newListArray (0, size - 1) elems        -- erzeugt ein mutable array aus elems
  withStorableArray arr $ \ptr -> bufferData buffTarg $= (ptrsize, ptr, buffUse) --erzeugt eine StateVar für das
  return array                                   -- return IO Objectname  

initBoidGraphics :: [SwarmFloat] -> [SwarmFloat] -> IO (BufferObject,BufferObject) 
initBoidGraphics vertArray vertIndexArray = do
  arrayName <- vboOfList ArrayBuffer        (length vertArray)      vertArray      StaticDraw  
  indexName <- vboOfList ElementArrayBuffer (length vertIndexArray) vertIndexArray StaticDraw
  let offset0 = wordPtrToPtr $ fromIntegral (0::Int)
  arrayPointer IndexArray  $= (VertexArrayDescriptor 1 Float 0 $ offset0)
  clientState  IndexArray  $= Enabled
  arrayPointer VertexArray $= (VertexArrayDescriptor 3 Float 0 $ offset0)
  clientState  VertexArray $= Enabled
  return (arrayName, indexName)

initOpenGl:: IORef GameWorld -> IO ()
initOpenGl gwRef = do 
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Swarm"

  shdrVert <- initShader VertexShader "./resources/ModelViewShader.vert"
  shdrFrag <- initShader FragmentShader "./resources/FragShader.frag" 
  _ <- installShaders [shdrVert, shdrFrag]

  depthFunc $= Just Less 
  keyboardMouseCallback $= Just (keyboardMouse)
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle gwRef)
  displayCallback $= display gwRef 
