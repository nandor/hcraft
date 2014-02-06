module HCraft.Renderer.Mesh.OBJ
  ( readObj
  ) where

import Text.Read
import Foreign.C.Types
import Control.Monad

import HCraft.Engine
import HCraft.Renderer.Mesh.Mesh

readObj :: String -> Engine MeshSource
readObj objSource = do
  return $ loadObj [] (lines objSource)
 where loadObj :: [Maybe Float] -> [String] -> MeshSource
       loadObj verts [] = 
         MeshSource $ join [[CFloat x] | (Just x) <- verts]
       loadObj verts (x:xs) =
         let objWords = words x
             -- multiple types of identifiers will use these values if they exist

             -- separated by a space after the identifier, there is the x value (or u value for texture coordinates)
             -- the y value (v value) is the next word after the x value
             -- z follows in the same way
             mx = readMaybe (head . tail $ objWords)
             my = readMaybe (head . tail . tail $ objWords)
             mz = readMaybe (head . tail . tail . tail $ objWords)
         in case objWords of
           -- verts ++ [..] preserves the order of the vertices
           "v" : ws -> loadObj (verts ++ [mx, my, mz]) xs
           -- skip this line and move on to the next line
           otherwise -> loadObj verts xs
