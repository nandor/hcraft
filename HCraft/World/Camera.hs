{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.World.Camera
  ( module HCraft.World.Camera.Camera
  , updateCamera
  , getCameraVolume
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Bits
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL hiding (Plane)
import HCraft.Engine
import HCraft.Math
import HCraft.World.Camera.Camera

updateCamera :: GLfloat -> Engine ()
updateCamera dt = do
  EngineState{..} <- ask

  -- Retrieve input
  size@(Size w h) <- liftIO $ get windowSize
  Position mx my <- liftIO $ get mousePos

  -- Retrieve camera info
  camera@Camera{..} <- liftIO $ get esCamera
  let Vec3 rx ry rz = cRotation
      Vec3 px py pz = cPosition

  -- Compute new parameters
  let aspect = fromIntegral w / fromIntegral h
      dx = fromIntegral (my - (h `shiftR` 1)) * dt
      dy = fromIntegral (mx - (w `shiftR` 1)) * dt
      rx' = max (min (rx + dx) (pi / 2 - abs dx)) (-pi / 2 + abs dx)
      ry' = ry - dy
      rz' = rz
      dir = Vec3 (sin ry' * cos rx') (sin rx') (cos ry' * cos rx')

  -- Possible move directions
  let dirs  = [ vinv dir
              , dir
              , Vec3{ v3x = sin (ry' - pi / 2) * cos rx'
                    , v3y = 0.0
                    , v3z = cos (ry' - pi / 2) * cos rx'
                    }
              , Vec3{ v3x = sin (ry' + pi / 2) * cos rx'
                    , v3y = 0.0
                    , v3z = cos (ry' + pi / 2) * cos rx'
                    }
              ]

  -- Sum up active directions
  dirs' <- forM (zip [ UP, DOWN, LEFT, RIGHT ] dirs) $
    \( key, dir ) -> do
      liftIO $ getKey key >>= \status -> return $ case status of
        Press -> dir
        Release -> Vec3 0 0 0

  let moveDir = foldr1 (^+^) dirs'
      moveDir' = if vlen moveDir < 0.1
                    then Vec3 0 0 0
                    else moveDir ^/. vlen moveDir
      pos' = cPosition ^+^ moveDir' ^*. 0.3

  -- Compute new orientation
  liftIO $ do
    mousePos $= Position (w `shiftR` 1) (h `shiftR` 1)
    esCamera $= camera
      { cPosition = pos'
      , cRotation = Vec3 rx' ry' rz'
      , cDirection = dir
      , cProjMat = mat4Persp (pi / 4) aspect 0.1 100.0
      , cViewMat = mat4LookAt pos' (pos' ^+^ dir) (Vec3 0 1 0)
      , cSkyMat = mat4LookAt vzero dir (Vec3 0 1 0)
      , cAspect = aspect
      }

getCameraVolume :: Camera -> Frustum
getCameraVolume Camera{..}
  = Frustum . map pnorm $
      [ Plane (Vec3 (m3 + m2) (m7 + m6) (m11 + m10)) (m15 + m14)
      , Plane (Vec3 (m3 - m2) (m7 - m6) (m11 - m10)) (m15 - m14)
      , Plane (Vec3 (m3 - m1) (m7 - m5) (m11 -  m9)) (m15 - m13)
      , Plane (Vec3 (m3 + m1) (m7 + m5) (m11 +  m9)) (m15 + m13)
      , Plane (Vec3 (m3 + m0) (m7 + m4) (m11 +  m8)) (m15 + m12)
      , Plane (Vec3 (m3 - m0) (m7 - m4) (m11 -  m8)) (m15 - m12)
      ]
  where
    Mat4 c0 c1 c2 c3 = cProjMat |*| cViewMat
    Vec4 m0 m4 m8 m12 = c0
    Vec4 m1 m5 m9 m13 = c1
    Vec4 m2 m6 m10 m14 = c2
    Vec4 m3 m7 m11 m15 = c3
