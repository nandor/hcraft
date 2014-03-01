module HCraft.World.Chunk.Chunk where

import           Data.IORef
import           Data.Vector.Storable.Mutable (IOVector)
import           Foreign
import           Foreign.C
import           Graphics.Rendering.OpenGL
import           HCraft.Math

data ChunkMesh
  = ChunkMesh{ cmVAO    :: VertexArrayObject
             , cmIBO    :: BufferObject
             , cmLength :: GLint
             }

-- List of blocks
data Block
  = Empty
  | Grass
  | Stone
  | Bedrock
  deriving (Eq, Ord, Show, Enum)

-- |Blocks are marshalled into 4 byte integers
instance Storable Block where
  alignment _ = alignment (4 :: CInt)
  sizeOf _ = 4
  peek p = do
    i <- peekByteOff p 0 :: IO CInt
    return (toEnum . fromIntegral $ i)
  poke p block = do
    pokeByteOff p 0 (fromEnum block)

-- |Chunks contain a list of blocks, a mesh and a texture which retains
--  the ids of blocks at a given coordinate
data Chunk
  = Chunk{ chMesh     :: IORef (Maybe ChunkMesh)
         , chVisible  :: IORef (Maybe QueryObject)
         , chDirty    :: IORef Bool
         , chBlocks   :: IOVector Block
         , chBlockTex :: TextureObject
         , chPosition :: Vec3 GLint
         , chModel    :: Mat4 GLfloat
         }

chunkSize :: Num a => a
chunkSize
  = fromIntegral 32
