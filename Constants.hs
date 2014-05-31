module Constants 
(minCoord
,maxCoord
,SwarmFloat
,SwarmM44)
where 

import Graphics.Rendering.OpenGL(GLfloat)
import Linear(M44)

maxCoord:: SwarmFloat
maxCoord = 10

minCoord:: SwarmFloat
minCoord = -maxCoord

type SwarmFloat = GLfloat

type SwarmM44 = M44 SwarmFloat
