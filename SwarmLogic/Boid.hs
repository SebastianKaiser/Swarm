module SwarmLogic.Boid 
(Boid
,Orientation
,position
,velocity
,orientation
,direction
,angle
,rotateBoid)
where

import Graphics.Rendering.OpenGL.Raw.Types
import Graphics.Rendering.OpenGL.GL.Tensor
import System.Random
import Debug.Trace

type Direction = Vector3 GLdouble

data Orientation = Orientation
    { angle :: GLdouble
    , direction :: Direction
    } deriving (Show)

data Boid = Boid
    { position :: Direction
    , velocity :: Direction
    , orientation :: Orientation
    } deriving (Show)

-- creation of random orientation
instance Random Orientation where
    random g =
        let (ang, g2) = random g
            (dir, ng) = random g2 in
        (Orientation ang dir, ng)

    randomR (min, max) g =
        let (ang, g2) = randomR (angle min, angle max) g
            (dir, ng) = randomR (direction min, direction max) g2 in
        (Orientation ang dir, ng)

-- creation of random boid
instance Random Boid where
    random g = 
        let (pos, g2) =  random g
            (vel, g3) =  random g2 
            (or, ng) =  random g3 in
        (Boid pos vel or, ng) 

    randomR (min, max) g = 
        let (pos, g2) =  randomR (position min, position max) g
            (vel, g3) =  randomR (velocity min, velocity max) g2
            (or, ng) = randomR (orientation min, orientation max) g3 in
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

rotateBoid :: Boid -> Boid
rotateBoid (Boid pos vel orient) =
    let newAngle = if angle orient < 1 
                   then (angle orient + 0.00001) 
                   else 0 :: GLdouble 
        dir = direction orient in
    (Boid pos vel (Orientation newAngle dir))
