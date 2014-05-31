module SwarmLogic.GameWorld
( GameWorld
, createGameWorld
, moveOn
, maxCoord
, minCoord
, getBoids
)
where

import Constants
import qualified Linear.V3 as Vect
import qualified Data.List as List
import System.Random
import SwarmLogic.SceneTree
import SwarmLogic.Boid

type GameWorld = [Boid]

-- code
createGameWorld :: Int -> StdGen -> GameWorld
createGameWorld n gen =    
    take n $ List.unfoldr (Just . (randomBoid)) gen

moveOn :: GameWorld -> GameWorld
moveOn gw = map rotateBoid gw

getBoids :: GameWorld -> GameWorld
getBoids gw = gw
