{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- 3D and 4D vectors
--------------------------------------------------------------------------------
module HCraft.Math.Vector where

import Control.Applicative

-- Vector typeclass
infix 7 ^.^, ^*^, ^*., .*^, ^/.
infix 6 ^+^, ^-^

class Vector a v where
  -- |Cross product
  (^*^) :: v a -> v a -> v a

  -- |Dot product
  (^.^) :: v a -> v a -> a

  -- |Vector addition
  (^+^) :: v a -> v a -> v a

  -- |Vector subtraction
  (^-^) :: v a -> v a -> v a

  -- |Scalar multiplication
  (^*.) :: v a -> a -> v a
  (.*^) :: a -> v a -> v a

  -- |Scalar division
  (^/.) :: v a -> a -> v a

  -- |Vector length
  vlen :: v a -> a

  -- |Negation
  vinv :: v a -> v a

  -- |Normalisation
  vnorm :: v a -> v a

  -- |Zero vector
  vzero :: v a

  -- |Constructs a vector from a list
  vfroml :: [ a ] -> v a

  -- |Constructs a list from a vector
  lfromv :: v a -> [ a ]

-- |3D Vector
data Vec3 a
  = Vec3 { v3x :: a
         , v3y :: a
         , v3z :: a
         }
  deriving ( Eq, Ord, Show )

instance Floating a => Vector a Vec3 where
  (Vec3 ax ay az) ^*^ (Vec3 bx by bz)
    = Vec3 (ay * bz - az * by)
           (az * bx - ax * bz)
           (ax * by - ay * bx)

  (Vec3 ax ay az) ^.^ (Vec3 bx by bz)
    = ax * bx + ay * by + az * bz

  (Vec3 ax ay az) ^+^ (Vec3 bx by bz)
    = Vec3 (ax + bx) (ay + by) (az + bz)

  (Vec3 ax ay az) ^-^ (Vec3 bx by bz)
    = Vec3 (ax - bx) (ay - by) (az - bz)

  (Vec3 ax ay az) ^*. x
    = Vec3 (ax * x) (ay * x) (az * x)

  x .*^ (Vec3 ax ay az)
    = Vec3 (x * ax) (x * ay) (x * az)

  (Vec3 ax ay az) ^/. x
    = Vec3 (ax / x) (ay / x) (az / x)

  vlen (Vec3 x y z)
    = sqrt (x * x + y * y + z * z)

  vinv (Vec3 x y z)
    = Vec3 (-x) (-y) (-z)

  vnorm v@(Vec3 x y z)
    = let l = vlen v in Vec3 (x / l) (y / l) (z / l)

  vzero
    = Vec3 0 0 0

  vfroml [ x, y, z ]
    = Vec3 x y z

  lfromv (Vec3 x y z)
    = [ x, y, z ]

instance Functor Vec3 where
  fmap f (Vec3 x y z)
    = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
  pure x
    = Vec3 x x x

  Vec3 f g h <*> Vec3 x y z
    = Vec3 (f x) (g y) (h z)

-- |4D Vector
data Vec4 a
  = Vec4 { v4x :: a
         , v4y :: a
         , v4z :: a
         , v4w :: a
         }
  deriving ( Eq, Ord, Show )

instance Floating a => Vector a Vec4 where
  (^*^)
    = undefined

  (Vec4 ax ay az aw) ^.^ (Vec4 bx by bz bw)
    = ax * bx + ay * by + az * bz + aw * bw

  (Vec4 ax ay az aw) ^+^ (Vec4 bx by bz bw)
    = Vec4 (ax + bx) (ay + by) (az + bz) (aw + bw)

  (Vec4 ax ay az aw) ^-^ (Vec4 bx by bz bw)
    = Vec4 (ax - bx) (ay - by) (az - bz) (aw - bw)

  (Vec4 ax ay az aw) ^*. x
    = Vec4 (ax * x) (ay * x) (az * x) (aw * x)

  x .*^ (Vec4 ax ay az aw)
    = Vec4 (x * ax) (x * ay) (x * az) (x * aw)

  (Vec4 ax ay az aw) ^/. x
    = Vec4 (ax / x) (ay / x) (az / x) (aw / x)

  vlen (Vec4 x y z w)
    = sqrt (x * x + y * y + z * z + w * w)

  vinv (Vec4 x y z w)
    = Vec4 (-x) (-y) (-z) (-w)

  vnorm v@(Vec4 x y z w)
    = let l = vlen v in Vec4 (x / l) (y / l) (z / l) (w / l)

  vzero
    = Vec4 0 0 0 0

  vfroml [ x, y, z, w ]
    = Vec4 x y z w

  lfromv (Vec4 x y z w)
    = [ x, y, z, w ]
