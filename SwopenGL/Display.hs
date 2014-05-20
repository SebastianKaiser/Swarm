module SwopenGL.Display (idle, display) where
 
import Graphics.UI.GLUT
import Data.IORef
import qualified SwarmLogic.GameWorld as GW
import qualified SwarmLogic.Boid as Boid
import Graphics.GLUtil.Camera3D
import Graphics.GLUtil.Linear
import Data.Maybe
import Linear.Matrix
import Linear.V3
import Linear.Quaternion

display :: IORef GW.GameWorld -> DisplayCallback
display gwRef = do
  gw <- get gwRef 
  clear [ColorBuffer, DepthBuffer] 
  let boids = GW.getBoids gw 

  prog <- get currentProgram
  modelViewLoc <- get $ uniformLocation (fromJust prog) "ModelView"

  let cam = dolly (V3 0 0 0) $ fpsCamera::Camera GLfloat
      camMat = camMatrix cam

  mapM_ (\boid -> do 
          let quat = axisAngle (Boid.direction . Boid.orientation $ boid) (Boid.angle . Boid.orientation $ boid) 
              model = mkTransformationMat eye3 (Boid.position boid)
              modelView  = camMat !*! model 
          modelView `asUniform` modelViewLoc 
          drawArrays LineLoop 0 3 ) boids              

  swapBuffers 

idle :: IORef GW.GameWorld -> IdleCallback
idle gwRef = do
  gw <- get gwRef
  writeIORef gwRef $ GW.moveOn gw
  postRedisplay Nothing
