module HCraft.World.Chunk.Noise where

import HCraft.World.Chunk.Block
import HCraft.Engine

noiseGetBlock :: Int -> Int -> Int -> Block
noiseGetBlock x y z
  | y < 5 = Stone
  | otherwise = Empty
