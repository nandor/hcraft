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
import Data.Map (Map)
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
                , esTextures   :: IORef (Map String TexObject)
                , esMeshes     :: IORef (Map String MeshObject)
                , esPrograms   :: IORef (Map String ProgObject)
                , esProgram    :: IORef (Maybe ProgObject)
                , esDfrFBO     :: IORef (Maybe FramebufferObject)
                , esFxFBO      :: IORef (Maybe FramebufferObject)
                , esShadowFBO  :: IORef (Maybe FramebufferObject)
                , esCamera     :: IORef Camera
                , esChunks     :: IORef (Map (Vec3 GLint) (IORef Chunk))
                , esCursor     :: IORef (Maybe ( Vec3 Int, Vec3 Int ))
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
