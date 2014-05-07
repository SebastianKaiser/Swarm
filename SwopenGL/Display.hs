module SwopenGL.Display (idle, display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import SwopenGL.Cube
import SwopenGL.Points
import qualified SwarmLogic.GameWorld as GW
import qualified SwarmLogic.Boid as Boid
import Data.Vect.Double
import Debug.Trace
import Data.List
 
display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef GW.GameWorld -> DisplayCallback
display angle pos gwRef = do
  gw <- get gwRef 
  clear [ColorBuffer, DepthBuffer] 
  loadIdentity
  preservingMatrix $ do
    scale 0.2 0.2 (0.2::GLfloat)
    let boids = GW.getBoids gw 
    forM_ boids $ \boid -> preservingMatrix $ do
----       rotate a $ GW.orientation boid
      translate $ Boid.position boid
      cube 0.1
  swapBuffers 

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing
