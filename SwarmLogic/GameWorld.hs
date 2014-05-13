module SwarmLogic.GameWorld
( GameWorld
, createGameWorld
, getBoids
, moveOn
, maxCoord
, minCoord
)
where

import Debug.Trace
import Debug.Hood.Observe
import Constants
import Graphics.Rendering.OpenGL.GL.Tensor
import qualified Data.Vect.Double as Vect
import Data.Vect.Double.OpenGL
import qualified SwarmLogic.Boid as Boid
import qualified Data.List as List
import System.Random
import qualified Data.Map as Map 
import Data.Map.Strict

-- Gameworld
data GameWorld = 
    GameWorld (Map Integer Boid.Boid) 
    (Map Integer Boid.Boid) 
    (Map Integer Boid.Boid) deriving (Show)

class GameAccess g where
    empty::g
    xmap::g -> Map Integer Boid.Boid
    ymap::g -> Map Integer Boid.Boid
    zmap::g -> Map Integer Boid.Boid
    getBoids::g -> [Boid.Boid]      

instance GameAccess GameWorld where
    empty = GameWorld Map.empty Map.empty Map.empty
    xmap (GameWorld xmap _ _) = xmap
    ymap (GameWorld _ ymap _) = ymap
    zmap (GameWorld _ _ zmap) = zmap
    getBoids (GameWorld xmap _ _) = elems xmap  

-- code
initGameWorld:: Int -> IO GameWorld
initGameWorld noOfBoids = 
    do
      gen  <- newStdGen
      return $ createGameWorld noOfBoids gen

randomBoid:: RandomGen g => g -> (Boid.Boid, g)
randomBoid g = randomR (Boid.minBoid, Boid.maxBoid) g
--     
-- let (ret, ng) =  in
-- trace (show ret) (ret, ng)
 
createGameWorld:: Int -> StdGen -> GameWorld
createGameWorld n gen =
    let rl = take n . List.unfoldr (Just . randomBoid ) in
    insertAllBoids $ rl gen   

deleteBoid:: Boid.Boid -> GameWorld -> GameWorld
deleteBoid boid gw =
    let Vect.Vec3 x y z = Boid.position boid 
        xm = deleteBoidC boid (\b -> x * 256) $ xmap gw
        ym = deleteBoidC boid (\b -> y * 256) $ ymap gw       
        zm = deleteBoidC boid (\b -> z * 256) $ zmap gw in
    GameWorld xm ym zm

deleteBoidC:: (RealFrac a) => Boid.Boid -> (Boid.Boid -> a) 
           -> Map Integer Boid.Boid -> Map Integer Boid.Boid
deleteBoidC boid f hm = 
    Map.delete (floor $ f boid) hm
    
insertAllBoids:: [Boid.Boid] -> GameWorld
insertAllBoids rs =
    List.foldl (\gw b -> insertBoid b gw) SwarmLogic.GameWorld.empty rs 

insertBoid:: Boid.Boid -> GameWorld -> GameWorld
insertBoid boid gw = 
    let Vect.Vec3 x y z = Boid.position boid in
    let xm = insertBoidC boid (\b -> x * 256) $ xmap gw
        ym = insertBoidC boid (\b -> y * 256) $ ymap gw       
        zm = insertBoidC boid (\b -> z * 256) $ zmap gw in
    GameWorld xm ym zm

insertBoidC:: (RealFrac a) => Boid.Boid -> (Boid.Boid -> a) 
           -> Map Integer Boid.Boid -> Map Integer Boid.Boid
insertBoidC boid f hm = 
    Map.insert (floor $ f boid) boid hm

moveOn:: GameWorld -> GameWorld
moveOn gw = 
    let boids = getBoids gw in
    insertAllBoids $ List.map (\b -> moveBoid b) boids

moveBoid:: Boid.Boid -> Boid.Boid
moveBoid boid = Boid.rotateBoid boid
