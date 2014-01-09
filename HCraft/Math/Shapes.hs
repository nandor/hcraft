{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.Math.Shapes where

import Graphics.Rendering.OpenGL hiding (Plane, Outside)
import HCraft.Math.Vector

class Shape a where
  outsidePlane :: a -> Plane -> Bool
  outsidePlane _ _
    = True

  outsideFrustum :: a -> Frustum -> Bool
  outsideFrustum _ _
    = True

  outsideBox :: a -> Box -> Bool
  outsideBox _ _
    = True

  insidePlane :: a -> Plane -> Bool
  insidePlane _ _
    = False
  insideFrustum :: a -> Frustum -> Bool
  insideFrustum _ _
    = False
  insideBox :: a -> Box -> Bool
  insideBox _ _
    = False

data Plane
  = Plane{ pNormal :: Vec3 GLfloat
         , pDist   :: GLfloat
         }
  deriving ( Eq, Ord, Show )

pnorm :: Plane -> Plane
pnorm Plane{..}
  = let len = vlen pNormal
    in Plane (pNormal ^/. len) (pDist / len)

data Frustum
  = Frustum{ fPlanes :: [ Plane ]
           }
  deriving ( Eq, Ord, Show )

data Box
  = Box{ bbMin :: Vec3 GLfloat
       , bbMax :: Vec3 GLfloat
       }
  deriving ( Eq, Ord, Show )

instance Shape Frustum where
  -- |Checks whether a frustum is outside a frustum
  outsideBox Frustum{..} (Box (Vec3 xl yl zl) (Vec3 xr yr zr))
    = any (\plane -> all (outside plane) points) fPlanes
    where
      outside Plane{..} point
        = pNormal ^.^ point + pDist < 0
      points
        = [ Vec3 xl yl zl, Vec3 xl yl zr, Vec3 xl yr zl, Vec3 xl yr zr
          , Vec3 xr yl zl, Vec3 xr yl zr, Vec3 xr yr zl, Vec3 xr yr zr
          ]

  -- |Checks whether a frustum is inside a frustum
  insideBox Frustum{..} (Box (Vec3 xl yl zl) (Vec3 xr yr zr))
    = all inside [ ( point, plane ) | plane <- fPlanes, point <- points ]
    where
      inside ( point, Plane{..} )
        = pNormal ^.^ point + pDist > 0
      points
        = [ Vec3 xl yl zl, Vec3 xl yl zr, Vec3 xl yr zl, Vec3 xl yr zr
          , Vec3 xr yl zl, Vec3 xr yl zr, Vec3 xr yr zl, Vec3 xr yr zr
          ]
