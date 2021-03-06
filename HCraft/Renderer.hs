{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.Renderer
  ( module Renderer
  , renderSky
  , renderChunks
  , renderEntitities
  , renderCursor
  ) where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.IORef
import           Data.List
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import qualified Data.Vector.Mutable as Vector
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW
import           GHC.Float
import           HCraft.Engine
import           HCraft.Math
import           HCraft.Renderer.Mesh as Renderer
import           HCraft.Renderer.Texture as Renderer
import           HCraft.Renderer.Program as Renderer
import           HCraft.World.Chunk
import           HCraft.World.Camera
import           HCraft.World.Cursor

-- |Renders the skybox
renderSky :: Engine ()
renderSky = do
  EngineState{..} <- ask
  Camera {..} <- liftIO $ get esCamera

  bindProgram "sky"
  bindTexture 1 "sky" "u_diffuse"
  parameterm "u_proj" cProjMat
  parameterm "u_view" cSkyMat
  renderMesh "skybox"

  liftIO $ clear [ DepthBuffer ]

-- |Orders chunks, placing the ones which are closer to the player first
orderChunks :: Vec3 GLint -> [ Chunk ] -> [ Chunk ]
orderChunks o
  = sortBy pred
  where
    o' = fromIntegral <$> o
    pred Chunk{ chPosition = v } Chunk{ chPosition = v' }
      = d `compare` d'
      where
        d = vlen $ (fromIntegral <$> v) ^-^ o'
        d' = vlen $ (fromIntegral <$> v') ^-^ o'

-- |Renders chunks
renderChunks :: Vec3 GLint -> Engine ()
renderChunks pos@(Vec3 x y z) = do
  EngineState{..} <- ask
  camera@Camera{..} <- liftIO $ get esCamera

  let volume = getCameraVolume camera
      boxMin = Vec3 (x - 0) 0 (z - 0)
      boxMax = Vec3 (x + 8) 4 (z + 8)

  -- The number of new chunks built each second is limited
  liftIO $ esCount $= 0

  -- Retrieve all the chunks which are inside the view
  -- volume & are inside the view range
  chunks <- orderChunks pos <$> selectChunks volume boxMin boxMax

  -- Render all the visible chunks
  -- Chunks which are definitely invisible (a previous occlusion query on them
  -- failed) are not rendered.
  bindProgram "terrain"
  parameterm "u_proj" cProjMat
  parameterm "u_view" cViewMat
  parameterv "u_pos" cPosition
  mapM_ renderChunk chunks

  -- Perform occlusion queries on all the chunks to find out which ones are
  -- going to be rendered next frame. A very simple & fast shader is used and
  -- writing to buffers is disabled for this pass.
  liftIO $ do
    colorMask $= Color4 Disabled Disabled Disabled Disabled
    depthMask $= Disabled
    cullFace $= Nothing

  bindProgram "depth"
  parameterm "u_proj" cProjMat
  parameterm "u_view" cViewMat
  mapM_ occludeChunk chunks

  liftIO $ do
    cullFace $= Just Back
    depthMask $= Enabled
    colorMask $= Color4 Enabled Enabled Enabled Enabled

-- |Uses view frustum culling & occlusion culling to select chunks
selectChunks :: Frustum -> Vec3 GLint -> Vec3 GLint -> Engine [ Chunk ]
selectChunks f v0@(Vec3 x0 y0 z0) v1@(Vec3 x1 y1 z1)
  | v0 == v1 = do
    chunk <- getChunk v0
    case chunk of
      Nothing -> return []
      Just x -> return [x]
  | otherwise = do
    -- The current box must be split along x, y and z only if the width on
    -- the given axis is larger than zero. If the two coordinates do not
    -- match, we can always find a way to split the box
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

    -- If a box is inside the view volume, we recurse
    chunks <- forM divs $ \( v0, v1 ) -> do
      let v0' = (*chunkSize) . fromIntegral <$> v0
          v1' = (*chunkSize) . (+1) . fromIntegral <$> v1

      if outsideBox f (Box v0' v1')
          then return []
          else selectChunks f v0 v1

    return (concat chunks)

-- |Renders all entities
renderEntitities :: Engine ()
renderEntitities = do
  return ()
