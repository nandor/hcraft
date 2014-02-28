{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HCraft.Engine
  ( module HCraft.World.Camera.Camera
  , EngineState(..)
  , Engine(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.IORef
import Data.HashMap.Strict (HashMap)
import Data.Vector.Mutable (IOVector)
import Graphics.Rendering.OpenGL
import HCraft.Renderer.Texture.Texture
import HCraft.Renderer.Program.Program
import HCraft.Renderer.Mesh.Mesh
import HCraft.World.Camera.Camera
import HCraft.World.Chunk.Chunk
import HCraft.Math

-- |Engine state
data EngineState
  = EngineState { esRunning    :: IORef Bool
                , esSize       :: IORef Size
                , esTextures   :: IORef (HashMap String TexObject)
                , esMeshes     :: IORef (HashMap String MeshObject)
                , esPrograms   :: IORef (HashMap String ProgObject)
                , esProgram    :: IORef (Maybe ProgObject)
                , esDfrFBO     :: IORef (Maybe FramebufferObject)
                , esFxFBO      :: IORef (Maybe FramebufferObject)
                , esShadowFBO  :: IORef (Maybe FramebufferObject)
                , esCamera     :: IORef Camera
                -- | Chunk HashMap. Keys: bottom left front corners of chunk bounding box in global chunk space
                , esChunks     :: IORef (HashMap (Vec3 Int) (IORef Chunk))
                , esCursor     :: IORef (Maybe ( Vec3 Int, Vec3 Int ))
                , esCount      :: IORef Int
                }

-- |Engine monad stack
newtype Engine a
  = Engine { run :: ErrorT String (ReaderT EngineState IO) a
           }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadError String
           , MonadIO
           , MonadReader EngineState
           )
