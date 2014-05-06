module SwarmLogic.GameWorld
(Boid,
 GameWorld,
 createGameWorld,
 getBoids,
 position,
 velocity,
 orientation)
where

import Graphics.Rendering.OpenGL.Raw.Types
import Graphics.Rendering.OpenGL.GL.Tensor
import qualified Data.List as List
import System.Random
import qualified Data.Map as Map 
import Data.Map.Strict
import Data.Traversable

type Position = Vector3 GLdouble
type Velocity = Vector3 GLdouble
type Orientation = Vector3 GLdouble
data Boid = Boid Position Velocity Orientation deriving (Show)

-- Boids need to deliver position and location and orientation
class HasBoidInfo b where
  position :: b -> Position
  velocity :: b -> Velocity
  orientation :: b -> Orientation

instance Random Boid where
  random g = 
      let (pos, g2) =  random g
          (vel, g3) =  random g2 
          (or, ng) =  random g3 in
      (Boid pos vel or, ng) 

  randomR (min, max) g = 
      let (pos, g2 ) =  randomR (position min, position max) g
          (vel, g3 ) =  randomR (velocity min, velocity max) g2
          (or, ng ) = randomR (orientation min, orientation max) g3 in
      (Boid pos vel or, ng) 

instance (Num a, Random a) => Random (Vector3 a) where
  random g = 
      let (x,g2) = random g
          (y,g3) = random g2
          (z,ng) = random g3        
      in                          
        (Vector3 x y z, ng)
               
  randomR (min, max) g = 
      let Vector3 min_x min_y min_z = min
          Vector3 max_x max_y max_z = max
      in
      let (x,g2) = randomR (min_x, max_x) g
          (y,g3) = randomR (min_y, max_y) g2
          (z,ng) = randomR (min_z, max_z) g3       
      in                          
        (Vector3 x y z, ng)

instance HasBoidInfo Boid where
  position (Boid position _ _) = position
  velocity (Boid _ velocity _) = velocity
  orientation (Boid _ _ orientation) = orientation

-- Gameworld
data GameWorld = 
    GameWorld (Map Integer Boid) 
    (Map Integer Boid) 
    (Map Integer Boid) deriving (Show)

class GameAccess g where
    empty::g
    xmap::g -> Map Integer Boid
    ymap::g -> Map Integer Boid
    zmap::g -> Map Integer Boid
    getBoids::g -> [Boid]      

instance GameAccess GameWorld where
    empty = GameWorld Map.empty Map.empty Map.empty
    xmap (GameWorld xmap _ _) = xmap
    ymap (GameWorld _ ymap _) = ymap
    zmap (GameWorld _ _ zmap) = zmap
    getBoids (GameWorld xmap _ _) = elems xmap  

-- code
initGameWorld:: Int -> IO GameWorld
initGameWorld noOfBoids = do
  gen  <- newStdGen
  return $ createGameWorld noOfBoids gen

randomlist:: (Random b) => Int -> StdGen -> [b]
randomlist n = take n . List.unfoldr (Just . random )

createGameWorld:: Int -> StdGen -> GameWorld
createGameWorld noOfBoids gen =  let rs = randomlist noOfBoids gen :: [Boid] in
                       List.foldl (\gw b -> insertBoid b gw) SwarmLogic.GameWorld.empty rs 

insertBoid:: Boid -> GameWorld -> GameWorld
insertBoid boid gw = 
    let Vector3 x y z = position boid in
    let xm = insertBoidC boid (\b -> x * 256) $ xmap gw
        ym = insertBoidC boid (\b -> y * 256) $ ymap gw       
        zm = insertBoidC boid (\b -> z * 256) $ zmap gw in
    GameWorld  xm ym zm

insertBoidC:: (RealFrac a) => Boid -> (Boid -> a) -> Map Integer Boid -> Map Integer Boid
insertBoidC boid f hm = 
    let key = f $ boid in
    Map.insert (floor key) boid hm













