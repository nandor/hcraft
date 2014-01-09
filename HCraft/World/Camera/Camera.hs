module HCraft.World.Camera.Camera where

import Graphics.Rendering.OpenGL
import HCraft.Math

data Camera
  = Camera { cPosition  :: Vec3 GLfloat
           , cRotation  :: Vec3 GLfloat
           , cDirection :: Vec3 GLfloat
           , cViewMat   :: Mat4 GLfloat
           , cProjMat   :: Mat4 GLfloat
           , cSkyMat    :: Mat4 GLfloat
           , cAspect    :: GLfloat
           , cFOV       :: GLfloat
           , cNearPlane :: GLfloat
           , cFarPlane  :: GLfloat
           }
  deriving ( Eq, Show )
