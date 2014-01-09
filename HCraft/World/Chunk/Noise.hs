module HCraft.World.Chunk.Noise where

import HCraft.World.Chunk.Block
import HCraft.Engine

getBlock :: Int -> Int -> Int -> Block
getBlock x y z
  | y < 5 = Stone
  | otherwise = Empty
