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
import qualified Data.Vect.Double as Vect
import System.Random

type Direction = Vect.Vec3

data Orientation = Orientation
    { angle :: Double
    , angVel :: Double
    , direction :: Direction
    } deriving (Show)

data Boid = Boid
    { position :: Direction
    , velocity :: Direction
    , orientation :: Orientation
    } deriving (Show)

maxBoid::Boid
maxBoid = Boid { position = 
                     (Vect.Vec3 maxCoord maxCoord 0),
                 velocity = 
                     Vect.Vec3 (0.5) (0.5) (0.5),
                 orientation = 
                     Orientation { angle = 359,
                                   angVel = 0.01,
                                   direction = (Vect.Vec3 maxCoord maxCoord 0)} 
               }

minBoid::Boid
minBoid = Boid { position = 
                     (Vect.Vec3 minCoord minCoord 0),
                 velocity = 
                     Vect.Vec3 (-0.5) (-0.5) (-0.5),
                 orientation = 
                     Orientation { angle = 0, 
                                   angVel = -0.01,
                                   direction = (Vect.Vec3 minCoord minCoord 0)} 
               }
                            
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

extractBoidValList:: [Boid] -> (Boid -> Vect.Vec3) -> Vect.Vec3
extractBoidValList xs f = sumVecList $ map f xs 

sumVecList:: [Vect.Vec3] -> Vect.Vec3
sumVecList xs = foldl (Vect.&+) Vect.zero xs

avgVecList:: [Vect.Vec3] -> Vect.Vec3
avgVecList xs = 
   let len = (fromIntegral $ length xs) in
        (1 / len) Vect.*& (sumVecList xs)    
