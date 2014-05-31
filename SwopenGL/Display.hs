module SwopenGL.Display (idle, display) where
 
import Graphics.UI.GLUT
import Data.IORef
import qualified SwarmLogic.GameWorld as GW
import qualified SwarmLogic.Boid as Boid
import qualified SwarmLogic.SceneTree as Scene
import Graphics.GLUtil.Camera3D
import Graphics.GLUtil.Linear
import Data.Maybe
import Linear.Matrix
import Linear.V3
import Linear.Quaternion
import SwopenGL.Cube

display :: IORef GW.GameWorld -> DisplayCallback
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
              rot     = fromQuaternion $ axisAngle (Boid.direction orient) (Boid.angle orient)
              tramat  = mkTransformationMat eye3 (Boid.position boid)
              rotmat  = mkTransformationMat rot (V3 0 0 0)
              model   = tramat !*! rotmat 
          model `asUniform` modelLoc 
          drawArrays LineLoop 0 3 ) $ gw              

  swapBuffers 

idle :: IORef GW.GameWorld -> IdleCallback
idle gwRef = do
  gw <- get gwRef
  writeIORef gwRef $ GW.moveOn gw
  postRedisplay Nothing
