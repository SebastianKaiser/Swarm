module SwarmLogic.GameWorld
( GameWorld,
 createGameWorld,
 getBoids)
where

import Graphics.Rendering.OpenGL.Raw.Types
import Graphics.Rendering.OpenGL.GL.Tensor
import SwarmLogic.Boid
import qualified Data.List as List
import System.Random
import qualified Data.Map as Map 
import Data.Map.Strict
import Data.Traversable


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













