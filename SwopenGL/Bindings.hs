module SwopenGL.Bindings (idle, display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Data.IORef
import SwopenGL.Display
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit ( exitWith, ExitCode(..) )
 
reshape :: ReshapeCallback
reshape (GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.perspective 120 (fromIntegral w / fromIntegral h) 0.1 (-100)
  GL.lookAt (Vertex3 0 0 0) (Vertex3 0 0 0) (Vector3 0 1 0)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _ _ _ = return ()
