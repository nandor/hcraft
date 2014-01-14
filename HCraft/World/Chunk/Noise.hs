module HCraft.World.Chunk.Noise
  ( noiseGetBlock
  ) where

import           Control.Monad.State
import           Data.Vector(Vector, (!))
import qualified Data.Vector as Vector
import           Debug.Trace
import           Graphics.Rendering.OpenGL
import           System.Random
import           HCraft.World.Chunk.Block
import           HCraft.Engine

-- |Gradient vectors
grad :: Vector ( GLfloat, GLfloat, GLfloat )
grad
  = Vector.fromList
    [ (  1,  1,  0 )
    , ( -1,  1,  0 )
    , (  1, -1,  0 )
    , ( -1, -1,  0 )
    , (  1,  0,  1 )
    , ( -1,  0,  1 )
    , (  1,  0, -1 )
    , ( -1,  0, -1 )
    , (  0,  1,  1 )
    , (  0, -1,  1 )
    , (  0,  1, -1 )
    , (  0, -1, -1 )
    ]

-- |Permutations table
perm :: Vector Int
perm
  = Vector.fromList . map fix $ perm' ++ perm'
  where
    fix i
      = abs i `mod` 256
    perm' = evalState (replicateM 256 (state random)) (mkStdGen 0)

-- |Permutation table mod 8
permMod12 :: Vector Int
permMod12
  = Vector.map (`mod` 12) perm

-- |Returns the noise value in a given point in space
simplexNoise :: GLfloat -> GLfloat -> GLfloat -> GLfloat
simplexNoise xin yin zin
  = 32 * (n0 + n1 + n2 + n3)
  where
    f3 = 1.0 / 3.0
    g3 = 1.0 / 6.0

    s = (xin + yin + zin) * f3;

    i = floor (xin + s)
    j = floor (yin + s)
    k = floor (zin + s)

    t = fromIntegral (i + j + k) * g3

    ( ( i1, j1, k1 ), ( i2, j2, k2 ) )
      | x0 >= y0 && y0 >= z0 = ( ( 1, 0, 0 ), ( 1, 1, 0 ) )
      | x0 >= y0 && x0 >= z0 = ( ( 1, 0, 0 ), ( 1, 0, 1 ) )
      | z0 >= y0             = ( ( 0, 0, 1 ), ( 1, 0, 1 ) )
      | y0 < z0              = ( ( 0, 0, 1 ), ( 0, 1, 1 ) )
      | x0 < z0              = ( ( 0, 1, 0 ), ( 0, 1, 1 ) )
      | otherwise            = ( ( 0, 1, 0 ), ( 1, 1, 0 ) )

    x0 = xin - fromIntegral i + t
    y0 = yin - fromIntegral j + t
    z0 = zin - fromIntegral k + t
    x1 = x0 - fromIntegral i1 + g3
    y1 = y0 - fromIntegral j1 + g3
    z1 = z0 - fromIntegral k1 + g3
    x2 = x0 - fromIntegral i2 + 2.0 * g3
    y2 = y0 - fromIntegral j2 + 2.0 * g3
    z2 = z0 - fromIntegral k2 + 2.0 * g3
    x3 = x0 - 1.0 + 3.0 * g3
    y3 = y0 - 1.0 + 3.0 * g3
    z3 = z0 - 1.0 + 3.0 * g3

    ii = i `mod` 256
    jj = j `mod` 256
    kk = k `mod` 256

    gi0 = permMod12 ! (ii + perm ! (jj + perm ! k))
    gi1 = permMod12 ! (ii + i1 + perm ! (jj + j1 + perm ! (k + k1)))
    gi2 = permMod12 ! (ii + i2 + perm ! (jj + j2 + perm ! (k + k2)))
    gi3 = permMod12 ! (ii +  1 + perm ! (jj +  1 + perm ! (k +  1)))

    n0
      | t < 0.0 = 0.0
      | otherwise = let t' = t * t in t' * t' * dot (grad ! gi0) x0 y0 z0
      where
        t = 0.6 - x0 * x0 - y0 * y0 - z0 * z0

    n1
      | t < 0.0 = 0.0
      | otherwise = let t' = t * t in t' * t' * dot (grad ! gi1) x1 y1 z1
      where
        t = 0.6 - z1 * x1 - y1 * y1 - z1 * z1

    n2
      | t < 0.0 = 0.0
      | otherwise = let t' = t * t in t' * t' * dot (grad ! gi2) x2 y2 z2
      where
        t = 0.6 - x2 * x2 - y2 * y2 - z2 * z2

    n3
      | t < 0.0 = 0.0
      | otherwise = let t' = t * t in t' * t' * dot (grad ! gi3) x3 y3 z3
      where
        t = 0.6 - x3 * x3 - y3 * y3 - z3 * z3

    dot ( x, y, z ) x' y' z'
      = x * x' + y * y' + z * z'

-- |Decides which block to place based on a given noise value
noiseGetBlock :: GLint -> GLint -> GLint -> Block
noiseGetBlock x y z
  | noise < 0.5 = Empty
  | otherwise = Stone
  where
    noise = simplexNoise x' y' z'
    x' = fromIntegral x / 20
    y' = fromIntegral y / 20
    z' = fromIntegral z / 20