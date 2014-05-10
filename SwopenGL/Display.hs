module SwopenGL.Display (idle, display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import SwopenGL.Cube
import SwopenGL.Points
import qualified SwarmLogic.GameWorld as GW
import qualified SwarmLogic.Boid as Boid
import Data.Vect.Double
import Data.Vect.Double.OpenGL
import Debug.Trace
import Data.List
 
display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef GW.GameWorld -> DisplayCallback
display angle pos gwRef = do
  gw <- get gwRef 
  clear [ColorBuffer, DepthBuffer] 
  loadIdentity
  preservingMatrix $ do
    scale 0.4 0.4 (0.4::GLfloat)
    let boids = GW.getBoids gw 
    forM_ boids $ \boid -> preservingMatrix $ do
      color $ Color3 (1::GLfloat) 1 1 
      let orient = Boid.orientation boid
      glTranslate $ Boid.position boid
      glRotate ((Boid.angle orient)) $ Boid.direction orient
      cube 0.1
      color $ Color3 (0::GLfloat) 0 0 -- set outline color to black
      cubeFrame 0.1 -- draw the outline
  swapBuffers 

idle :: IORef GW.GameWorld -> IdleCallback
idle gwRef = do
  gw <- get gwRef
  writeIORef gwRef $ GW.moveOn gw
  postRedisplay Nothing
