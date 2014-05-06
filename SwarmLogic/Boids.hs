module SwarmLogic.Boids (
sumVecList, 
avgVecList )  
where

import SwarmLogic.GameWorld as GW
import Data.Vect.Double
import Data.List
import System.Random

-- pure
extractBoidValList:: [Boid] -> (Boid -> Vec3) -> Vec3
extractBoidValList xs f = sumVecList $ map f xs 

sumVecList:: [Vec3] -> Vec3
sumVecList xs = foldl (&+) zero xs

avgVecList:: [Vec3] -> Vec3
avgVecList xs = 
   let len = (fromIntegral $ length xs) in
        (1 / len) *& (sumVecList xs)    
