module SwopenGL.Display (idle, display) where
 
import Graphics.UI.GLUT
import Data.IORef
import qualified SwarmLogic.GameWorld as GW
import qualified SwarmLogic.Boid as Boid
import Graphics.GLUtil.Camera3D
import Graphics.GLUtil.Linear
import Data.Maybe
import Linear.Matrix
import Linear.V4

display :: IORef GW.GameWorld -> DisplayCallback
display gwRef = do
  gw <- get gwRef 
  clear [ColorBuffer, DepthBuffer] 
  let boids = GW.getBoids gw 

  prog <- get currentProgram
  modelViewLoc <- get $ uniformLocation (fromJust prog) "ModelView"
  let camMat          = camMatrix $ (fpsCamera ::Camera GLfloat)
      model           = V4 (V4 1 0 0 (0::GLfloat)) 
                           (V4 0 1 0 (0::GLfloat)) 
                           (V4 0 0 1 (0::GLfloat)) 
                           (V4 0 0 0 (1::GLfloat))    
      modelView       = model !*! camMat

  modelView `asUniform` modelViewLoc 

  drawArrays LineLoop 0 3
  swapBuffers 

idle :: IORef GW.GameWorld -> IdleCallback
idle gwRef = do
  gw <- get gwRef
  --print $ show $ Boid.avgVecList $ map Boid.position $ GW.getBoids gw
  writeIORef gwRef $ GW.moveOn gw
  postRedisplay Nothing
