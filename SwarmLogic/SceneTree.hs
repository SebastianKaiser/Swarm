module SwarmLogic.SceneTree 
(Direction
,Position
,Orientation(..)
,Graphics
)

where

import Constants
import Data.Tree
import Linear(V3)
import Linear.Matrix
import Graphics.Rendering.OpenGL(GLfloat
								,VertexArrayDescriptor
								,BufferObject)
-- import Graphics.Rendering.OpenGL.GL.VertexArrays

type Direction  = V3 SwarmFloat
type Position   = V3 SwarmFloat
type Graphics 	= String

data Orientation = Orientation
    { angVel    :: SwarmFloat
    , angle     :: SwarmFloat
    , direction :: Direction
    } deriving (Show)

data SwarmNode = Root 
				| Mesh 	{ nodes    		:: [Position]					
						, vbo 			:: BufferObject		
						, vertArrayDesc :: VertexArrayDescriptor GLfloat
						, texture		:: String
						}

createTree :: a -> Tree a
createTree so = Node so []
