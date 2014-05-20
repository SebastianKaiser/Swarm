module SwarmLogic.Boid 
(Boid
,Orientation
,position
,velocity
,orientation
,direction
,angle
,rotateBoid
,minBoid
,maxBoid
,avgVecList
)
where

import Constants
import qualified Linear as Vect
import System.Random
import Graphics.Rendering.OpenGL(GLfloat)

type Direction = Vect.V3 GLfloat

data Orientation = Orientation
    { angle :: GLfloat
    , angVel :: GLfloat
    , direction :: Direction
    } deriving (Show)

data Boid = Boid
    { position :: Direction
    , velocity :: Direction
    , orientation :: Orientation
    } deriving (Show)

maxBoid::Boid
maxBoid = Boid { position = 
                     (Vect.V3 maxCoord maxCoord 0),
                 velocity = 
                     Vect.V3 (0.5) (0.5) (0.5),
                 orientation = 
                     Orientation { angle = 359,
                                   angVel = 0.01,
                                   direction = (Vect.V3 maxCoord maxCoord 0)} 
               }

minBoid::Boid
minBoid = Boid { position = 
                     (Vect.V3 minCoord minCoord 0),
                 velocity = 
                     Vect.V3 (-0.5) (-0.5) (-0.5),
                 orientation = 
                     Orientation { angle = 0, 
                                   angVel = -0.01,
                                   direction = (Vect.V3 minCoord minCoord 0)} 
               }
                            
instance (Random t) => Random (Vect.V3 t) where
    random g = 
        let (x, g2) = random g
            (y, g3) = random g2
            (z, ng) = random g3 in
        (Vect.V3 x y z, ng)

    randomR (minVal, maxVal) g =
        let (Vect.V3 minx miny minz) = minVal
            (Vect.V3 maxx maxy maxz) = maxVal
            (x, g2) = randomR (minx, maxx) g 
            (y, g3) = randomR (miny, maxy) g2 
            (z, ng) = randomR (minz, maxz) g3 in
        (Vect.V3 x y z, ng)
        
-- creation of random orientation
instance Random Orientation where
    random g =
        let (ang, g2) = random g
            (anv, g3) = random g2
            (dir, ng) = random g3 in
        (Orientation ang anv dir,  ng)

    randomR (minO, maxO) g =
        let (ang, g2) = randomR (angle minO, angle maxO) g
            (anv, g3) = randomR (angVel minO, angVel maxO) g2
            (dir, ng) = randomR (direction minO, direction maxO) g3 in
        (Orientation ang anv dir, ng)

-- creation of random boid
instance Random Boid where
    random g = 
        let (pos, g2) = random g
            (vel, g3) = random g2 
            (orient, ng) =  random g3 in
        (Boid pos vel orient, ng) 

    randomR (minB, maxB) g = 
        let (pos, g2) = randomR (position minB, position maxB) g
            (vel, g3) = randomR (velocity minB, velocity maxB) g2
            (orient, ng) =  randomR (orientation minB, orientation maxB) g3 in
        (Boid pos vel orient, ng) 

rotateBoid :: Boid -> Boid
rotateBoid (Boid pos vel orient) =
    let newAngle = if angle orient < 360 
                   then (angle orient + angVel orient) 
                   else 0 
        newAngVel = angVel orient
        dir = direction orient in
    (Boid pos vel (Orientation newAngle newAngVel dir))

extractBoidValList:: [Boid] -> (Boid -> Vect.V3 GLfloat) -> Vect.V3 GLfloat
extractBoidValList xs f = sumVecList $ map f xs 

sumVecList:: [Vect.V3 GLfloat] -> Vect.V3 GLfloat
sumVecList xs = Vect.sumV xs

avgVecList:: [Vect.V3 GLfloat] -> Vect.V3 GLfloat
avgVecList xs = 
   let len = (fromIntegral $ length xs) in
        (1 / len) Vect.*^ (sumVecList xs)    
