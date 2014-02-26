{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
--------------------------------------------------------------------------------
-- 3x3 and 4x4 matrices
--------------------------------------------------------------------------------
module HCraft.Math.Matrix where

import HCraft.Math.Vector

infix 7 |*|, |*., .*|, |/., |*^, ^*|
infix 6 |+|, |-|

-- |All operators consist of three characters,
--  the middle one representing the operation type,
--  the left and right encode the operand types:
--  '|' represents a matrix, whilst '.' represents
--  a scalar value
class Matrix a m where
  (|*|) :: m a -> m a -> m a
  (|*.) :: m a -> a -> m a
  (.*|) :: a -> m a -> m a
  (|/.) :: m a -> a -> m a
  (|+|) :: m a -> m a -> m a
  (|-|) :: m a -> m a -> m a

  mdet :: m a -> a
  mtrans :: m a -> m a
  mident :: m a
  lfromm :: m a -> [ a ]
  mfroml :: [ a ] -> m a

-- |Typeclass which is intended to be implemented
--  by vectors to support the multiplication of
--  4D matrices with vectors
class MatrixMultiplier a b where
  (|*^) :: Mat4 b -> a b -> a b
  (^*|) :: a b -> Mat4 b -> a b

-- |3D Matrix
data Mat3 a
  = Mat3 { m3x :: !(Vec3 a)
         , m3y :: !(Vec3 a)
         , m3z :: !(Vec3 a)
         }
  deriving ( Eq, Show )

-- |4D Matrix
data Mat4 a
  = Mat4 { m4x :: !(Vec4 a)
         , m4y :: !(Vec4 a)
         , m4z :: !(Vec4 a)
         , m4w :: !(Vec4 a)
         }
  deriving ( Eq, Show )

instance Floating a => Matrix a Mat3 where
  (|*|) (Mat3 (Vec3 a00 a01 a02) (Vec3 a10 a11 a12) (Vec3 a20 a21 a22))
        (Mat3 (Vec3 b00 b01 b02) (Vec3 b10 b11 b12) (Vec3 b20 b21 b22))
    = Mat3 (Vec3 (a00 * b00 + a10 * b01 + a20 * b02)
                 (a00 * b10 + a10 * b11 + a20 * b12)
                 (a00 * b20 + a10 * b21 + a20 * b22))
           (Vec3 (a01 * b00 + a11 * b01 + a21 * b02)
                 (a01 * b10 + a11 * b11 + a21 * b12)
                 (a01 * b20 + a11 * b21 + a21 * b22))
           (Vec3 (a02 * b00 + a12 * b01 + a22 * b02)
                 (a02 * b10 + a12 * b11 + a22 * b12)
                 (a02 * b20 + a12 * b21 + a22 * b22))

  (Mat3 x y z) |*. s
    = Mat3 (x ^*. s) (y ^*. s) (z ^*. s)

  s .*| (Mat3 x y z)
    = Mat3 (s .*^ x) (s .*^ y) (s .*^ z)

  (Mat3 x y z) |/. s
    = Mat3 (x ^/. s) (y ^/. s) (z ^/. s)

  (|+|) (Mat3 ax ay az) (Mat3 bx by bz)
    = Mat3 (ax ^+^ bx) (ay ^+^ by) (az ^+^ bz)

  (|-|) (Mat3 ax ay az) (Mat3 bx by bz)
    = Mat3 (ax ^-^ bx) (ay ^-^ by) (az ^-^ bz)

  mdet (Mat3 (Vec3 a00 a01 a02) (Vec3 a10 a11 a12) (Vec3 a20 a21 a22))
    = a00 * a11 * a22 + a10 * a21 * a02 + a01 * a12 * a20 -
      a20 * a11 * a02 - a10 * a01 * a22 - a00 * a21 * a12

  mtrans (Mat3 (Vec3 a00 a01 a02) (Vec3 a10 a11 a12) (Vec3 a20 a21 a22))
    = Mat3 (Vec3 a00 a10 a20) (Vec3 a01 a11 a21) (Vec3 a02 a12 a22)

  mident
    = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)

  mfroml [ a00, a01, a02 , a10, a11, a12 , a20, a21, a22 ]
    = Mat3 (Vec3 a00 a01 a02) (Vec3 a10 a11 a12) (Vec3 a20 a21 a22)

  lfromm (Mat3 x y z)
    = lfromv x ++ lfromv y ++ lfromv z

instance Floating a => Matrix a Mat4 where
  (|*|) (Mat4 (Vec4 a00 a01 a02 a03) (Vec4 a10 a11 a12 a13)
              (Vec4 a20 a21 a22 a23) (Vec4 a30 a31 a32 a33))
        (Mat4 (Vec4 b00 b01 b02 b03) (Vec4 b10 b11 b12 b13)
              (Vec4 b20 b21 b22 b23) (Vec4 b30 b31 b32 b33))
    = Mat4 (Vec4 (a00 * b00 + a10 * b01 + a20 * b02 + a30 * b03)
                 (a00 * b10 + a10 * b11 + a20 * b12 + a30 * b13)
                 (a00 * b20 + a10 * b21 + a20 * b22 + a30 * b23)
                 (a00 * b30 + a10 * b31 + a20 * b32 + a30 * b33))
           (Vec4 (a01 * b00 + a11 * b01 + a21 * b02 + a31 * b03)
                 (a01 * b10 + a11 * b11 + a21 * b12 + a31 * b13)
                 (a01 * b20 + a11 * b21 + a21 * b22 + a31 * b23)
                 (a01 * b30 + a11 * b31 + a21 * b32 + a31 * b33))
           (Vec4 (a02 * b00 + a12 * b01 + a22 * b02 + a32 * b03)
                 (a02 * b10 + a12 * b11 + a22 * b12 + a32 * b13)
                 (a02 * b20 + a12 * b21 + a22 * b22 + a32 * b23)
                 (a02 * b30 + a12 * b31 + a22 * b32 + a32 * b33))
           (Vec4 (a03 * b00 + a13 * b01 + a23 * b02 + a33 * b03)
                 (a03 * b10 + a13 * b11 + a23 * b12 + a33 * b13)
                 (a03 * b20 + a13 * b21 + a23 * b22 + a33 * b23)
                 (a03 * b30 + a13 * b31 + a23 * b32 + a33 * b33))

  (Mat4 x y z w) |*. s
    = Mat4 (x ^*. s) (y ^*. s) (z ^*. s) (w ^*. s)

  s .*| (Mat4 x y z w)
    = Mat4 (s .*^ x) (s .*^ y) (s .*^ z) (s .*^ w)

  (Mat4 x y z w) |/. s
    = Mat4 (x ^/. s) (y ^/. s) (z ^/. s) (w ^/. s)

  (|+|) (Mat4 ax ay az aw) (Mat4 bx by bz bw)
    = Mat4 (ax ^+^ bx) (ay ^+^ by) (az ^+^ bz) (aw ^+^ bw)

  (|-|) (Mat4 ax ay az aw) (Mat4 bx by bz bw)
    = Mat4 (ax ^-^ bx) (ay ^-^ by) (az ^-^ bz) (aw ^+^ bw)

  mdet
    = error "Not implemented"

  mtrans (Mat4 (Vec4 a00 a01 a02 a03) (Vec4 a10 a11 a12 a13)
               (Vec4 a20 a21 a22 a23) (Vec4 a30 a31 a32 a33))
    = Mat4 (Vec4 a00 a10 a20 a30) (Vec4 a01 a11 a21 a31)
           (Vec4 a02 a12 a22 a32) (Vec4 a03 a13 a23 a33)

  mident
    = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0)
           (Vec4 0 0 1 0) (Vec4 0 0 0 1)

  mfroml [ a00, a01, a02, a03 , a10, a11, a12, a13
         , a20, a21, a22, a23 , a30, a31, a32, a33
         ]
    = Mat4 (Vec4 a00 a01 a02 a03) (Vec4 a10 a11 a12 a13)
           (Vec4 a20 a21 a22 a23) (Vec4 a30 a31 a32 a33)

  lfromm (Mat4 x y z w)
    = lfromv x ++ lfromv y ++ lfromv z ++ lfromv w

-- |Allow multiplication of 3D vectors with 4x4 matrices
instance Floating a => MatrixMultiplier Vec3 a where
  (|*^) (Mat4 (Vec4 a00 a01 a02 a03) (Vec4 a10 a11 a12 a13)
              (Vec4 a20 a21 a22 a23) (Vec4 a30 a31 a32 a33))
        (Vec3 x y z)
    = Vec3 (a00 * x + a10 * y + a20 * z + a30)
           (a01 * x + a11 * y + a21 * z + a31)
           (a02 * x + a12 * y + a22 * z + a32)
  (^*|) (Vec3 x y z)  (Mat4 (Vec4 a00 a01 a02 a03)
                            (Vec4 a10 a11 a12 a13)
                            (Vec4 a20 a21 a22 a23)
                            (Vec4 a30 a31 a32 a33))
    = Vec3 (a00 * x + a01 * y + a02 * z + a03)
           (a10 * x + a11 * y + a12 * z + a13)
           (a20 * x + a21 * y + a22 * z + a23)

-- |Allow multiplication of 4D vectors with 4x4 matrices
instance Floating a => MatrixMultiplier Vec4 a where
  (|*^) (Mat4 (Vec4 a00 a01 a02 a03) (Vec4 a10 a11 a12 a13)
              (Vec4 a20 a21 a22 a23) (Vec4 a30 a31 a32 a33))
        (Vec4 x y z w)
    = Vec4 (a00 * x + a10 * y + a20 * z + a30 * w)
           (a01 * x + a11 * y + a21 * z + a31 * w)
           (a02 * x + a12 * y + a22 * z + a32 * w)
           (a03 * x + a13 * y + a23 * z + a33 * w)
  (^*|) (Vec4 x y z w)  (Mat4 (Vec4 a00 a01 a02 a03)
                              (Vec4 a10 a11 a12 a13)
                              (Vec4 a20 a21 a22 a23)
                              (Vec4 a30 a31 a32 a33))
    = Vec4 (a00 * x + a01 * y + a02 * z + a03 * w)
           (a10 * x + a11 * y + a12 * z + a13 * w)
           (a20 * x + a21 * y + a22 * z + a23 * w)
           (a30 * x + a31 * y + a32 * z + a33 * w)

-- |Creates a translation matrix
mat4Trans :: Num a => Vec3 a -> Mat4 a
mat4Trans (Vec3 x y z)
  = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 x y z 1)

-- |Creates a perspective projection matrix
mat4Persp :: Floating a => a -> a -> a -> a -> Mat4 a
mat4Persp fov a n f
  = Mat4 (Vec4 (1 / (a * t))     0.0             0.0    0.0)
         (Vec4           0.0 (1 / t)             0.0    0.0)
         (Vec4           0.0     0.0   ((f + n) / d) (-1.0))
         (Vec4           0.0     0.0 (2 * n * f / d)    0.0)
  where
    t = tan (fov / 2)
    d = n - f

-- |Creates an ortographic projection matrix
mat4Ortho :: Floating a => a -> a -> a -> a -> a -> a -> Mat4 a
mat4Ortho l r b t n f
  = Mat4 (Vec4      (2.0 / w)             0.0            0.0 0.0)
         (Vec4            0.0       (2.0 / h)            0.0 0.0)
         (Vec4            0.0             0.0     (-2.0 / d) 0.0)
         (Vec4 (-(r + l) / w)  (-(t + b) / h) (-(f + n) / d) 1.0)
  where
    w = r - l
    h = t - b
    d = f - n

-- |Creates a 4D view matrixhpl
mat4LookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat4 a
mat4LookAt eye at up
  = Mat4 (Vec4 xx yx zx 0) (Vec4 xy yy zy 0)
         (Vec4 xz yz zz 0) (Vec4 x' y' z' 1)
  where
    z@(Vec3 zx zy zz) = vnorm $ at ^-^ eye
    x@(Vec3 xx xy xz) = vnorm $ up ^*^ z
    y@(Vec3 yx yy yz) = z ^*^ x

    x' = -(x ^.^ eye)
    y' = -(y ^.^ eye)
    z' = -(z ^.^ eye)
