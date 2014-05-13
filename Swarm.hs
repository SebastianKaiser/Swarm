import Graphics.UI.GLUT
import Data.IORef
import SwopenGL.Bindings
import System.Random
import SwarmLogic.GameWorld
import Graphics.Rendering.OpenGL.GL.Shaders
import qualified Data.ByteString as BS
import System.Exit
import System.IO
import Control.Monad (when, unless)

main :: IO ()
main = do
  -- Gameworld
  gen  <- newStdGen
  let gw = createGameWorld 50 gen
  gwRef <- newIORef gw
  initOpenGl gwRef  
  prog <- createProgram 
  shdrVert <- initShader VertexShader "./Brick.vert" 
  shdrFrag <- initShader FragmentShader "./Brick.frag" 
  installShaders [shdrVert, shdrFrag]
  mainLoop

initShader::  ShaderType -> FilePath -> IO Shader
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

installShaders :: [Shader] -> IO ()
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

   let setUniform var val = do
       location <- get (uniformLocation prog var)
       reportErrors
       uniform location $= val

   setUniform "BrickColor" (Color3 1.0 0.3 (0.2 :: GLfloat))
   setUniform "MortarColor" (Color3 0.85 0.86 (0.84 :: GLfloat))
   setUniform "BrickSize" (Vertex2 0.30 (0.15 :: GLfloat))
   setUniform "BrickPct" (Vertex2 0.90 (0.85 :: GLfloat))
   setUniform "LightPosition" (Vertex3 0 0 (4 :: GLfloat))


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
