{-# OPTIONS_GHC -fno-warn-orphans #-}

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
,randomBoid
,boidGraphics
,maxCoord
,minCoord
)

where

import Constants
import System.Random
import Debug.Trace
import qualified Linear as Vect

data Boid = Boid
    { graphics    :: Graphics
    , position    :: Constants.Position
    , velocity    :: Direction
    , orientation :: Orientation
    } 

maxBoid::Boid  
maxBoid = Boid {   graphics    = boidGraphics, 
                   position    = Vect.V3 maxCoord maxCoord maxCoord,
                   velocity    = Vect.V3 (0.5) (0.5) (0.5),
                   orientation = Orientation { angVel    = 0.0001,
                                               angle     = 0.03,
                                               direction = (Vect.V3 maxCoord maxCoord 0)
                                             } 
               }

minBoid::Boid
minBoid = Boid {   graphics    = boidGraphics,
                   position    = (Vect.V3 minCoord minCoord minCoord),
                   velocity    = Vect.V3 (-0.5) (-0.5) (-0.5),
                   orientation = Orientation { angVel    = -0.0001,
                                               angle     = -0.03,
                                               direction = (Vect.V3 minCoord minCoord 0)
                                             } 
               }

vertArray :: [SwarmFloat]
vertArray =  [0, 0, 0, 
              0, 1, 0,
              1, 0, 0]

vertIndexArray :: [SwarmFloat]
vertIndexArray =  [3, 2, 1]

boidGraphics :: Graphics
boidGraphics = Graphics { vertexArray  = vertArray
                        , indexArray   = vertIndexArray}
                            
-- Random instance for V3                            
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
        (Boid boidGraphics pos vel orient, ng) 

    randomR (minB, maxB) g = 
        let (pos, g2) = randomR (position minB, position maxB) g
            (vel, g3) = randomR (velocity minB, velocity maxB) g2
            (orient, ng) =  randomR (orientation minB, orientation maxB) g3 in
        (Boid boidGraphics pos vel orient, ng) 

instance Show Boid where
    show boid = "Boid " 
                ++ (show $ position boid) ++ "\n " 
                ++ (show $ velocity boid) ++ "\n" 
                ++ (show $ orientation boid)

randomBoid :: RandomGen g => g -> (Boid, g)
randomBoid g = randomR (minBoid, maxBoid) g

rotateBoid :: Boid -> Boid
rotateBoid (Boid _ pos vel orient) =
    let (Orientation angV ang dir ) = orient
        newAng  = if ang < 1 then (ang + 0.0001) else 0 
        newBoid = Boid boidGraphics pos vel (Orientation angV newAng dir) in
    traceShow newBoid newBoid 

sumVecList :: [Vect.V3 SwarmFloat] -> Vect.V3 SwarmFloat
sumVecList xs = Vect.sumV xs

avgVecList :: [Vect.V3 SwarmFloat] -> Vect.V3 SwarmFloat
avgVecList xs = 
   let len = (fromIntegral $ length xs) in
        (1 / len) Vect.*^ (sumVecList xs)    
