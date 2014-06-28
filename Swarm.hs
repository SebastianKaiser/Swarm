import Graphics.UI.GLUT
import Data.IORef
import SwopenGL.Bindings
import SwarmLogic.GameWorld
import System.Random
import SwarmLogic.Boid
import Constants

main :: IO ()
main = do
  -- Gameworld
  gen  <- newStdGen
  let gw = createGameWorld 1 gen

  newIORef gw >>= initOpenGl  

  (arrayName, indexName) <- initBoidGraphics (vertexArray boidGraphics) (indexArray boidGraphics)

  mainLoop


