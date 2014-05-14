module SwopenGL.Bindings (
idle
, display
, reshape
, keyboardMouse
, initShader
, installShaders
) where
 
import Graphics.UI.GLUT
import Data.IORef
import SwopenGL.Display
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit ( exitWith, ExitCode(..) )
import Graphics.Rendering.OpenGL.GL.Shaders
import qualified Data.ByteString as BS
import System.Exit
import System.IO
import Control.Monad (when, unless)
import Graphics.GLUtil.Camera3D
import Graphics.GLUtil.Linear
import Data.Maybe
 
reshape :: ReshapeCallback
reshape (GL.Size w h) = do
  prog <- get currentProgram
  projLoc <- get $ uniformLocation (fromJust prog) "Projection"
  let ratio = (fromIntegral w)/(fromIntegral h)::GLfloat
  asUniform (projectionMatrix (1/2) ratio 0.1 100) projLoc
  let cam = fpsCamera :: Camera GLfloat
  camLoc <- get $ uniformLocation (fromJust prog) "Camera"
  asUniform (camMatrix cam) camLoc
  
  

  -- GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  -- GL.perspective 120 (fromIntegral w / fromIntegral h) 0.1 (-100)
  -- GL.lookAt (Vertex3 0 0 0) (Vertex3 0 0 0) (Vector3 0 1 0)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _ _ _ = return ()

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
                  
   let setUniform var val = do
                           location <- get $ uniformLocation prog var
                           reportErrors
                           uniform location $= val
                  

   setUniform "BrickColor" (Color3 1.0 0.3 (0.2 :: GLfloat))
   setUniform "MortarColor" (Color3 0.85 0.86 (0.84 :: GLfloat))
   setUniform "BrickSize" (Vertex2 0.30 (0.15 :: GLfloat))
   setUniform "BrickPct" (Vertex2 0.90 (0.85 :: GLfloat))
   -- setUniform "LightPosition" (Vertex3 0 0 (4 :: GLfloat))
   return prog

-- setUniform:: Program -> String -> a -> IO ()
-- setUniform prog var val = do
--        location <- get $ uniformLocation prog var
--        reportErrors
--        uniform location $= val
