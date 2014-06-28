module Constants 
(minCoord
,maxCoord
,SwarmFloat
,SwarmInt
,SwarmM44
,Direction
,Position
,Graphics(..)
,Orientation(..))
where 

import Graphics.Rendering.OpenGL(GLfloat, GLint)
import Linear(M44, V3)

maxCoord:: SwarmFloat
maxCoord = 1

minCoord:: SwarmFloat
minCoord = -maxCoord

type SwarmFloat = GLfloat
type SwarmInt 	= GLint
type SwarmM44 = M44 SwarmFloat

type Direction  = V3 SwarmFloat
type Position   = V3 SwarmFloat
data Graphics 	= Graphics { vertexArray:: [SwarmFloat]
							,indexArray :: [SwarmFloat] }
data Orientation = Orientation
    { angVel    :: SwarmFloat
    , angle     :: SwarmFloat
    , direction :: Direction
    } deriving (Show)
