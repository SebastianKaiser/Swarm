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

  mapM_ (\boid -> do 
          let model  = mkTransformationMat eye3 (Boid.position boid)
              modelView  = model !*! camMat
          modelView `asUniform` modelViewLoc 
          drawArrays LineLoop 0 3 ) boids              

  swapBuffers 

idle :: IORef GW.GameWorld -> IdleCallback
idle gwRef = do
  gw <- get gwRef
  writeIORef gwRef $ GW.moveOn gw
  postRedisplay Nothing
