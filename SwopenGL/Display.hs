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
  modelLoc <- get $ uniformLocation (fromJust prog) "Model"
  viewLoc <- get $ uniformLocation (fromJust prog) "View"

  let cam = dolly (V3 0 0 0) $ fpsCamera::Camera GLfloat
      camMat = camMatrix cam
  camMat `asUniform` viewLoc

  mapM_ (\boid -> do 
          let quat = axisAngle (Boid.direction . Boid.orientation $ boid) (Boid.angle . Boid.orientation $ boid) 
              model = mkTransformation quat (Boid.position boid)
              --model = mkTransformationMat eye3 (Boid.position boid)
              modelView  = camMat !*! model 
          model `asUniform` modelLoc 
          drawArrays LineLoop 0 3 ) boids              

  swapBuffers 

idle :: IORef GW.GameWorld -> IdleCallback
idle gwRef = do
  gw <- get gwRef
  writeIORef gwRef $ GW.moveOn gw
  postRedisplay Nothing
