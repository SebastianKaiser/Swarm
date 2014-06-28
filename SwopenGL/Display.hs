module SwopenGL.Display (idle, display) where
 
import Graphics.UI.GLUT
import Data.IORef
import qualified SwarmLogic.Boid as Boid
import Graphics.GLUtil.Camera3D
import Graphics.GLUtil.Linear
import Data.Maybe
import Linear.Matrix
import qualified Linear.Metric as Metric (normalize)
import Linear.V3
import Linear.Quaternion
import SwopenGL.Cube
import SwarmLogic.GameWorld

display :: IORef GameWorld -> DisplayCallback
display gwRef = do
  gw <- get gwRef 
  clear [ColorBuffer, DepthBuffer] 

  prog     <- get currentProgram
  modelLoc <- get $ uniformLocation (fromJust prog) "Model"
  viewLoc  <- get $ uniformLocation (fromJust prog) "View"

  let cam = dolly (V3 0 0 50) $ fpsCamera::Camera GLfloat
      camMat = camMatrix cam
  camMat `asUniform` viewLoc

  (eye4 :: M44 GLfloat) `asUniform` modelLoc
  cube 0.2

  mapM_ (\boid -> do
          let orient  = Boid.orientation boid
              rot     = fromQuaternion $ Metric.normalize $ axisAngle (Boid.direction orient) (Boid.angle orient)
              model   = mkTransformationMat rot $ Boid.position boid
          model `asUniform` modelLoc 
          -- threadDelay 100
          drawArrays LineLoop 0 3 ) $ gw              

  swapBuffers 

idle :: IORef GameWorld -> IdleCallback
idle gwRef = do
  gw <- get gwRef
  writeIORef gwRef $ moveOn gw
  postRedisplay Nothing
