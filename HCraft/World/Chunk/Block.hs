{-# LANGUAGE ForeignFunctionInterface #-}
module HCraft.World.Chunk.Block where

import Control.Applicative
import Foreign
import Foreign.C

data Block
  = Empty
  | Dirt
  | Stone
  deriving ( Eq, Ord, Show, Enum )

-- |Blocks are marshalled into 4 byte integers
instance Storable Block where
  alignment _ = alignment (4 :: CInt)
  sizeOf _ = 4
  peek p = do
    i <- peekByteOff p 0 :: IO CInt
    return (toEnum . fromIntegral $ i)
  poke p block = do
    pokeByteOff p 0 (fromEnum block)
