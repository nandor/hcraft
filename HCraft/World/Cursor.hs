{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.World.Cursor
  ( renderCursor
  , updateCursor
  ) where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.IORef
import           Data.Fixed
import qualified Data.Map as Map
import qualified Data.Vector.Mutable as Vector
import           Graphics.Rendering.OpenGL
import           HCraft.Engine
import           HCraft.Renderer.Program
import           HCraft.Renderer.Texture
import           HCraft.Renderer.Mesh
import           HCraft.Math
import           HCraft.World.Chunk

-- |Renders the cursor
renderCursor :: Engine ()
renderCursor = do
  EngineState{..} <- ask
  Camera{..} <- liftIO $ get esCamera
  liftIO (get esCursor) >>= \cursor -> case cursor of
    Nothing -> return ()
    Just ( pos, dir ) -> do
      liftIO $ blend $= Enabled

      bindProgram "object"
      bindTexture 0 "cursor" "u_diffuse"
      parameterv "u_proj" cProjMat
      parameterv "u_view" cViewMat
      parameterv "u_mdl" $ mat4Trans (fromIntegral <$> pos)
      renderMesh "cursor"

      liftIO $ blend $= Disabled

-- |Computes the cursors position by finding the first non-empty block directly
--  in front of the camera. Uses a fast voxel ray tracing algorithm
updateCursor :: Engine ()
updateCursor = do
  EngineState{..} <- ask
  Camera{..} <- liftIO $ get esCamera
  chunks <- liftIO $ get esChunks

  let camDir = negate <$> cDirection
      pos@(Vec3 x y z) = fromIntegral . floor <$> cPosition
      tmin@(Vec3 tx ty tz) = intbound <$> cPosition <*> camDir
      step@(Vec3 sx sy sz) = signum <$> camDir
      dir@(Vec3 dx dy dz) = (/) <$> step <*> camDir

      intbound p d
        | d < 0 = intbound (-p) (-d)
        | otherwise = (1 - (p `mod'` 1 + 1) `mod'` 1) / d

      trace pos'@(Vec3 x y z) t@(Vec3 tx ty tz) dir
        | vlen (pos' ^-^ pos) > 6 = return Nothing
        | otherwise = do
          let chunk = (`div` chunkSize) . floor <$> pos'
              Vec3 x' y' z' = (`mod` chunkSize) . floor <$> pos'
              idx = (x' * chunkSize + y') * chunkSize + z'

          block <- getBlock (floor <$> pos')
          case block of
            Just block ->
              return (Just ( floor <$> pos', floor <$> dir ) )
            _ | tx < ty && tx < tz ->
              trace (Vec3 (x + sx) y z) (Vec3 (tx + dx) ty tz) (Vec3 (-sx) 0 0)
            _ | tx >= ty && ty < tz ->
              trace (Vec3 x (y + sy) z) (Vec3 tx (ty + dy) tz) (Vec3 0 (-sy) 0)
            _ ->
              trace (Vec3 x y (z + sz)) (Vec3 tx ty (tz + dz)) (Vec3 0 0 (-sz))

  cursor <- trace pos tmin (Vec3 0 0 0)
  liftIO $ esCursor $= cursor
