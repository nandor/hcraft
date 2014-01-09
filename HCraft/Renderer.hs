{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.Renderer
  ( module Renderer
  , renderSky
  , renderChunks
  , renderEntitities
  ) where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Vector.Mutable as Vector
import           Graphics.Rendering.OpenGL
import           HCraft.Engine
import           HCraft.Math
import           HCraft.Renderer.Mesh as Renderer
import           HCraft.Renderer.Texture as Renderer
import           HCraft.Renderer.Program as Renderer
import           HCraft.World.Chunk
import           HCraft.World.Camera

-- |Renders the skybox
renderSky :: Engine ()
renderSky = do
  EngineState{..} <- ask
  Camera {..} <- liftIO $ get esCamera

  bindProgram "sky"
  bindTexture 1 "sky" "u_diffuse"
  parameterv "u_proj" cProjMat
  parameterv "u_view" cSkyMat
  renderMesh "skybox"

  liftIO $ clear [ DepthBuffer ]

-- |Renders chunks
renderChunks :: Vec3 GLint -> Engine ()
renderChunks (Vec3 x y z) = do
  EngineState{..} <- ask
  camera@Camera{..} <- liftIO $ get esCamera

  let volume = getCameraVolume camera
      boxMin = Vec3 (x - 8) 0 (z - 8)
      boxMax = Vec3 (x + 8) 8 (z + 8)

  -- Retrieve all the chunks inside the view volume
  chunks <- selectChunks volume boxMin boxMax

  liftIO $ esCounter $= 0

  -- Render all the visible chunks
  -- Chunks which are definitely invisible (a previous occlusion query on them
  -- failed) are not rendered
  bindProgram "object"
  bindTexture 1 "random" "u_diffuse"
  parameterv "u_proj" cProjMat
  parameterv "u_view" cViewMat
  mapM_ renderChunk chunks

  -- Perform occlusion queries on all the chunks to find out which ones are
  -- going to be rendered next frame
  bindProgram "depth"
  parameterv "u_proj" cProjMat
  parameterv "u_view" cViewMat
  mapM_ occludeChunk chunks

  liftIO $ get esCounter >>= print

-- |Uses view frustum culling & occlusion culling to select chunks
selectChunks :: Frustum -> Vec3 GLint -> Vec3 GLint -> Engine [ Chunk ]
selectChunks f v0@(Vec3 x0 y0 z0) v1@(Vec3 x1 y1 z1)
  | v0 == v1 = do
    EngineState{..} <- ask
    chunks <- liftIO $ get esChunks

    -- Retrieve the chunk from the cache
    chunk <- liftIO $ case Map.lookup v0 chunks of
      Nothing -> do
        chunk <- Chunk <$> newIORef Nothing
                       <*> newIORef Nothing
                       <*> Vector.new 10
                       <*> pure v0
                       <*> pure (mat4Trans $ fromIntegral <$> v0)
        ref <- newIORef chunk
        esChunks $= Map.insert v0 ref chunks
        return chunk
      Just x -> get x
    return [ chunk ]
  | otherwise = do
    let divs = [ ( v0, v1 ) ] >>= split v3x cx >>= split v3y cy >>= split v3z cz

        cx (Vec3 x y z) v = Vec3 v y z
        cy (Vec3 x y z) v = Vec3 x v z
        cz (Vec3 x y z) v = Vec3 x y v

        split s f ( v0, v1)
          | s v0 >= s v1 = [ ( v0, v1 ) ]
          | otherwise = let d = (s v1 - s v0) `div` 2
                        in [ ( v0, f v1 (s v0 + d) )
                           , ( f v0 (s v0 + d + 1), v1 )
                           ]

    chunks <- forM divs $ \( v0, v1 ) -> do
      let v0' = fromIntegral <$> v0
          v1' = (+1) . fromIntegral <$> v1

      if outsideBox f (Box v0' v1')
          then return []
          else selectChunks f v0 v1

    return (concat chunks)

-- |Renders all entities
renderEntitities :: Engine ()
renderEntitities = do
  return ()
