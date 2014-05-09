module SwarmLogic.GameWorld
( GameWorld
, createGameWorld
, getBoids
, moveOn
)
where

import Debug.Hood.Observe
import Graphics.Rendering.OpenGL.Raw.Types
import Graphics.Rendering.OpenGL.GL.Tensor
import qualified SwarmLogic.Boid as Boid
import qualified Data.List as List
import System.Random
import qualified Data.Map as Map 
import Data.Map.Strict
import Data.Traversable
import Debug.Trace

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

createGameWorld:: Int -> StdGen -> GameWorld
createGameWorld n gen =
    let rl = take n . List.unfoldr (Just . random ) in
    insertAllBoids $ rl gen   

deleteBoid:: Boid.Boid -> GameWorld -> GameWorld
deleteBoid boid gw =
    let Vector3 x y z = Boid.position boid 
        xm = deleteBoidC boid (\b -> x * maxCoordinate) $ xmap gw
        ym = deleteBoidC boid (\b -> y * maxCoordinate) $ ymap gw       
        zm = deleteBoidC boid (\b -> z * maxCoordinate) $ zmap gw in
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
    let Vector3 x y z = Boid.position boid in
    let xm = insertBoidC boid (\b -> x * maxCoordinate) $ xmap gw
        ym = insertBoidC boid (\b -> y * maxCoordinate) $ ymap gw       
        zm = insertBoidC boid (\b -> z * maxCoordinate) $ zmap gw in
    GameWorld xm ym zm

insertBoidC:: (RealFrac a) => Boid.Boid -> (Boid.Boid -> a) 
           -> Map Integer Boid.Boid -> Map Integer Boid.Boid
insertBoidC boid f hm = 
    Map.insert (floor $ f boid) boid hm

maxCoordinate::GLdouble
maxCoordinate = 512

moveOn:: GameWorld -> GameWorld
moveOn gw = 
    let boids = getBoids gw in
    insertAllBoids $ List.map (\b -> Boid.rotateBoid b) boids
