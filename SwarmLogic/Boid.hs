module SwarmLogic.Boid 
( Boid,
 position,
 velocity,
 orientation)
where

import Graphics.Rendering.OpenGL.Raw.Types
import Graphics.Rendering.OpenGL.GL.Tensor
import System.Random

type Position = Vector3 GLdouble
type Velocity = Vector3 GLdouble
type Orientation = Vector3 GLdouble
data Boid = Boid Position Velocity Orientation deriving (Show)

-- Boids need to deliver position and location and orientation
class HasBoidInfo b where
  position :: b -> Position
  velocity :: b -> Velocity
  orientation :: b -> Orientation

-- creation of random boid
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

-- Random instance for Vector3  
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

