{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.World.Chunk
  ( module Chunk
  , createChunk
  , renderChunk
  , occludeChunk
  , deleteChunk
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.IORef
import           Data.Maybe
import qualified Data.Vector.Mutable as Vector
import           Foreign
import           Graphics.Rendering.OpenGL
import           HCraft.World.Chunk.Block as Chunk
import           HCraft.World.Chunk.Chunk as Chunk
import           HCraft.World.Chunk.Noise as Chunk
import           HCraft.Engine
import           HCraft.Math
import           HCraft.Renderer.Program
import           HCraft.Renderer.Mesh

-- |Creates a new chunk
createChunk :: Vec3 GLint -> Engine ()
createChunk pos@(Vec3 x y z) = do
  return ()

-- |Renders a chunk
renderChunk :: Chunk -> Engine ()
renderChunk Chunk{..} = do
  query <- liftIO $ get chVisible

  case query of
    Nothing -> do
      --parameterv "u_mdl" chModel
      --renderMesh "chunk"
      return ()
    Just q -> do
      x <- liftIO $ get (queryResult q :: GettableStateVar GLint)
      when (x /= 0) $ do
        EngineState{..} <- ask
        liftIO $ esCounter $~! (+1)
      liftIO $ beginConditionalRender q QueryWait
      parameterv "u_mdl" chModel
      renderMesh "chunk"
      liftIO $ endConditionalRender

-- |Perform an occlusion query on the chunk
occludeChunk :: Chunk -> Engine ()
occludeChunk Chunk{..} = do
  query <- liftIO $ get chVisible
  object <- case query of
    Just obj -> return obj
    Nothing -> liftIO $ do
      obj <- genObjectName
      chVisible $= Just obj
      return obj

  liftIO $ do
    beginQuery AnySamplesPassed object
    colorMask $= Color4 Disabled Disabled Disabled Disabled
    depthMask $= Disabled
    cullFace $= Nothing

  parameterv "u_mdl" chModel
  renderMesh "chunkOccluder"

  liftIO $ do
    cullFace $= Just Back
    depthMask $= Enabled
    colorMask $= Color4 Enabled Enabled Enabled Enabled
    endQuery AnySamplesPassed

-- |Deletes a chunk
deleteChunk :: Int -> Engine ()
deleteChunk idx = do
  return ()