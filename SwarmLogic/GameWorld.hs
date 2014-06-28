module SwarmLogic.GameWorld (
GameWorld
,createGameWorld
,moveOn
,getBoids
,Boid(..))

where

import SwarmLogic.Boid
import System.Random (StdGen)
import Data.List

type GameWorld = [Boid]

-- code
createGameWorld :: Int -> StdGen -> GameWorld
createGameWorld n gen = 
    take n $ unfoldr (Just . (randomBoid)) gen

moveOn :: GameWorld -> GameWorld
moveOn gw = map rotateBoid gw

getBoids :: GameWorld -> GameWorld
getBoids gw = gw
