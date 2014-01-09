module HCraft.World.Chunk.Block where

data Block
  = Empty
  | Dirt
  | Stone
  deriving ( Eq, Ord, Show, Enum )