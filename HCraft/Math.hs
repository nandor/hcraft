module HCraft.Math
  ( module Math
  , fract
  ) where

import HCraft.Math.Matrix as Math
import HCraft.Math.Shapes as Math
import HCraft.Math.Vector as Math

-- |Fractional part of a number
fract :: RealFrac a => a -> a
fract x
  = x - fromIntegral (floor x :: Int)
