module HCraft.World.Chunk.Chunk where

import           Data.IORef
import           Data.Vector.Mutable (IOVector)
import           Graphics.Rendering.OpenGL
import           HCraft.Math
import           HCraft.World.Chunk.Block

data ChunkMesh
  = ChunkMesh{ cmVAO    :: VertexArrayObject
             , cmVBO    :: BufferObject
             , cmLength :: GLint
             }

data Chunk
  = Chunk{ chMesh     :: IORef (Maybe ChunkMesh)
         , chVisible  :: IORef (Maybe QueryObject)
         , chData     :: IOVector Block
         , chPosition :: Vec3 GLint
         , chModel    :: Mat4 GLfloat
         }
