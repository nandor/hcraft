module HCraft.World.Chunk.Chunk where

import           Data.IORef
import           Data.Vector.Storable.Mutable (IOVector)
import           Graphics.Rendering.OpenGL
import           HCraft.Math

data ChunkMesh
  = ChunkMesh{ cmVAO    :: VertexArrayObject
             , cmIBO    :: BufferObject
             , cmLength :: GLint
             }

data Chunk
  = Chunk{ chMesh     :: IORef (Maybe ChunkMesh)
         , chVisible  :: IORef (Maybe QueryObject)
         , chDirty    :: IORef Bool
         , chBlocks   :: IOVector Int
         , chBlockTex :: TextureObject
         , chPosition :: Vec3 GLint
         , chModel    :: Mat4 GLfloat
         }

chunkSize :: Num a => a
chunkSize
  = fromIntegral 32
