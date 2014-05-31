module SwopenGL.Bindings (
idle
, display
, reshape
, keyboardMouse
, initShader
, installShaders
, setProjMatrix
) where
 
import Graphics.UI.GLUT
import SwopenGL.Display
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit ( exitWith, ExitCode(..) )
import Graphics.Rendering.OpenGL.GL.Shaders
import qualified Data.ByteString as BS
import System.IO
import Control.Monad (when, unless)
import Graphics.GLUtil.Camera3D
import Graphics.GLUtil.Linear
import Data.Maybe
 
reshape :: ReshapeCallback
reshape (GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  setProjMatrix (GL.Size w h)

setProjMatrix :: Size -> IO ()
setProjMatrix (GL.Size w h) = do
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
   let setUniform var val = do
                           loc <- get $ uniformLocation prog var
                           reportErrors
                           uniform loc $= val
  --   setUniform "BrickColor" (Color3 1.0 0.3 (0.2 :: GLfloat))
  --   setUniform "MortarColor" (Color3 0.85 0.86 (0.84 :: GLfloat))
  --   setUniform "BrickSize" (Vertex2 0.30 (0.15 :: GLfloat))
  --   setUniform "BrickPct" (Vertex2 0.90 (0.85 :: GLfloat))
  --   setUniform "LightPosition" (Vertex3 0 0 (4 :: GLfloat))
   return prog
