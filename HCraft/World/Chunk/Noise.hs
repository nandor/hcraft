{-# LANGUAGE ForeignFunctionInterface #-}
module HCraft.World.Chunk.Noise
  ( noiseGetBlock
  ) where

import           Control.Monad.State
import           Data.Vector(Vector, (!))
import qualified Data.Vector as Vector
import           Debug.Trace
import           Foreign.C.Types
import           Graphics.Rendering.OpenGL
import           System.Random
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
    perm' = evalState (replicateM 256 (state random)) (mkStdGen 1)

-- |Permutation table mod 8
permMod12 :: Vector Int
permMod12
  = Vector.map (`mod` 12) perm

-- |Returns the noise value in a given point in space
foreign import ccall unsafe "simplex.h simplexNoise"
  simplexNoise :: CFloat -> CFloat -> CFloat -> CFloat

-- |Decides which block to place based on a given noise value
noiseGetBlock :: GLint -> GLint -> GLint -> Int
noiseGetBlock x y z
  | noise < 0.5 = 0
  | otherwise = 1
  where
    noise = simplexNoise x' y' z'
    x' = fromIntegral x / 20
    y' = fromIntegral y / 40
    z' = fromIntegral z / 20
