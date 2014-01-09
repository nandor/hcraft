{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.Renderer.Mesh
  ( module Mesh
  , buildMesh
  , renderMesh
  ) where

import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import qualified Data.Map as Map
import           Foreign
import           Graphics.Rendering.OpenGL
import           System.FilePath
import           HCraft.Renderer.Mesh.Mesh as Mesh
import           HCraft.Renderer.Mesh.OBJ as Mesh
import           HCraft.Engine

-- | Builds a mesh from a source array
buildMesh :: MeshDesc -> Engine ()
buildMesh MeshDesc{..} = do
  -- Retrieve mesh data
  MeshSource{..} <- case mdSource of
    Right src -> return src
    Left file -> case takeExtension file of
      ".o" -> readObj file
      _ -> fail $ "Cannot load '" ++ file ++ "'"

  -- Build the mesh
  ( vao, vbo, len ) <- liftIO $ do
    vao <- genObjectName
    vbo <- genObjectName

    bindVertexArrayObject $= Just vao
    bindBuffer ArrayBuffer $= Just vbo

    withArrayLen msVertices $ \len ptr -> do
      bufferData ArrayBuffer $= ( fromIntegral len * 4, ptr, StreamDraw )

      vertexAttribArray (AttribLocation 0) $= Enabled
      vertexAttribPointer (AttribLocation 0) $=
        ( ToFloat, VertexArrayDescriptor 3 Float 32 (intPtrToPtr 0 ) )

      vertexAttribArray (AttribLocation 1) $= Enabled
      vertexAttribPointer (AttribLocation 1) $=
        ( ToFloat, VertexArrayDescriptor 3 Float 32 (intPtrToPtr 12) )

      vertexAttribArray (AttribLocation 2) $= Enabled
      vertexAttribPointer (AttribLocation 2) $=
        ( ToFloat, VertexArrayDescriptor 2 Float 32 (intPtrToPtr 24) )

      return ( vao, vbo, fromIntegral len )

  EngineState{..} <- ask
  liftIO $ get esMeshes >>= \cache ->
    let obj = MeshObject vao vbo len
    in esMeshes $= Map.insert mdName obj cache

-- | Renders a mesh from the cache
renderMesh :: String -> Engine ()
renderMesh name = do
  EngineState{..} <- ask

  liftIO $ get esMeshes >>= \cache -> case Map.lookup name cache of
    Nothing -> fail $ "Mesh not found '" ++ name ++ "'"
    Just MeshObject{..} -> do
      bindVertexArrayObject $= Just moVAO
      drawArrays Triangles 0 (fromIntegral moLength)
