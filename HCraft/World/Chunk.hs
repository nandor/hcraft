{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.World.Chunk
  ( module Chunk
  , getChunk
  , buildChunk
  , renderChunk
  , occludeChunk
  , deleteChunk
  , getBlock
  , placeBlock
  , deleteBlock
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.IORef
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Vector.Mutable as Vector
import           Debug.Trace
import           Foreign
import           Graphics.Rendering.OpenGL
import           System.Random
import           HCraft.World.Chunk.Block as Chunk
import           HCraft.World.Chunk.Chunk as Chunk
import           HCraft.World.Chunk.Noise as Chunk
import           HCraft.Engine
import           HCraft.Math
import           HCraft.Renderer.Program
import           HCraft.Renderer.Mesh

-- |Array containing the coordinates of all the cubes
coords :: (Enum a, Num a) => [ ( a, a, a ) ]
coords
  = [( x, y, z ) | x <- [0..15], y <- [0..15], z <- [0..15]]

-- |Creates a new chunk or retrieves it from the cache
getChunk :: Vec3 GLint -> Engine Chunk
getChunk pos = do
  EngineState{..} <- ask
  chunks <- liftIO $ get esChunks

  liftIO $ case Map.lookup pos chunks of
    Nothing -> do
      -- The new chunk does not have a VAO or query object attached yet
      -- They are built when the chunk is first rendered. Lazy initialization
      -- is used because when building meshes for chunks data from neighbours
      -- might be required, but we do not want their meshes to be built yet
      chunk <- Chunk <$> newIORef Nothing
                     <*> newIORef Nothing
                     <*> newIORef True
                     <*> Vector.new (16 * 16 * 16)
                     <*> pure pos
                     <*> pure (mat4Trans $ (*16) . fromIntegral <$> pos)

      -- Fill in the chunk with random data
      forM_ coords $ \( x, y, z) -> do
        rnd <- liftIO $ randomIO :: IO Int
        block <- if rnd `mod` 4 /= 0
                  then return Empty
                  else return Stone
        liftIO $ Vector.write (chBlocks chunk) (((x * 16) + y) * 16 + z) block

      ref <- newIORef chunk
      esChunks $= Map.insert pos ref chunks
      return chunk
    Just x -> get x

-- |Builds the mesh for a chunk
buildChunk :: Chunk -> Engine [ GLuint ]
buildChunk Chunk{..} = do
  arr <- forM [(pt, f) | pt <- coords, f <- [0..5]] $ \( ( x, y, z ), f ) -> do
    let idx  = (x * 16 + y) * 16 + z
        mesh = (idx * 6 + f) * 4
    block <- liftIO $ Vector.read chBlocks idx
    return $ if block /= Empty
              then map (\i -> fromIntegral (mesh + i)) [0, 1, 2, 2, 1, 3]
              else []
  return (concat arr)

-- |Renders a chunk
renderChunk :: Chunk -> Engine ()
renderChunk chunk@Chunk{..} = do
  EngineState{..} <- ask

  -- If a chunk intersects the camera, the occlusion query is disabled
  inCamera <- liftIO (get esCamera) >>= \Camera{..} ->
    let pos = (fromIntegral . floor . (/16) <$> cPosition)
        Vec3 x y z = pos ^-^ (fromIntegral <$> chPosition)
    in return $ (x + y + z) <= 2

  -- If a chunk was modified, delete its mesh and query object
  -- The mesh is going to be rebuilt this frame and will only be rendered
  -- next frme
  dirty <- liftIO $ get chDirty
  when dirty $ liftIO $ do
    chDirty $= False
    get chVisible >>= \query ->
      when (isJust query) $
        deleteObjectName (fromJust query)
    get chMesh >>= \mesh ->
      when (isJust mesh) $ do
        deleteObjectName (cmVAO $ fromJust mesh)
        deleteObjectName (cmIBO $ fromJust mesh)

    chVisible $= Nothing
    chMesh $= Nothing

  -- If the chunk was already built and did not change, redraw it.
  -- Otherwise, a new mesh is built at this point
  -- The chunk is only rendered if it is not occluded or contains the camera
  query <- liftIO $ get chVisible
  mesh <- liftIO $ get chMesh
  when (isJust query || inCamera) $ do
    unless inCamera . liftIO $
      beginConditionalRender (fromJust query) QueryWait

    parameterv "u_mdl" chModel

    case mesh of
      Just ChunkMesh{..} -> liftIO $ do
        bindVertexArrayObject $= Just cmVAO
        drawElements Triangles (fromIntegral cmLength) UnsignedInt nullPtr
      Nothing -> do
        vao <- liftIO genObjectName
        ibo <- liftIO genObjectName

        -- Vertex data
        liftIO $ do
          bindVertexArrayObject $= Just vao
          get esMeshes >>= \cache -> case Map.lookup "chunkMesh" cache of
            Nothing -> fail "Mesh not found 'chunkMesh'"
            Just MeshObject{..} ->
              bindBuffer ArrayBuffer $= Just moVBO

          vertexAttribArray (AttribLocation 0) $= Enabled
          vertexAttribPointer (AttribLocation 0) $=
            ( ToFloat, VertexArrayDescriptor 3 Float 32 (intPtrToPtr 0 ) )

          vertexAttribArray (AttribLocation 1) $= Enabled
          vertexAttribPointer (AttribLocation 1) $=
            ( ToFloat, VertexArrayDescriptor 3 Float 32 (intPtrToPtr 12) )

          vertexAttribArray (AttribLocation 2) $= Enabled
          vertexAttribPointer (AttribLocation 2) $=
            ( ToFloat, VertexArrayDescriptor 2 Float 32 (intPtrToPtr 24) )

        indices <- buildChunk chunk

        -- Index data
        liftIO $ do
          bindBuffer ElementArrayBuffer $= Just ibo

          len <- withArrayLen indices $ \len ptr -> do
            let len' = fromIntegral len
            bufferData ElementArrayBuffer $= ( len' * 4, ptr, StreamDraw )
            return (fromIntegral len)

          chMesh $= Just (ChunkMesh vao ibo len)
          drawElements Triangles len UnsignedInt nullPtr

    unless inCamera . liftIO $
      endConditionalRender

-- |Perform an occlusion query on the chunk
occludeChunk :: Chunk -> Engine ()
occludeChunk Chunk{..} = do
  query <- liftIO $ get chVisible
  object <- case query of
    Just obj -> return obj
    Nothing -> liftIO $ do
      obj <- genObjectName
      chVisible $= Just obj
      return obj

  liftIO $ beginQuery AnySamplesPassed object
  parameterv "u_mdl" chModel
  renderMesh "chunkOccluder"
  liftIO $ endQuery AnySamplesPassed

-- |Retrieves a block
getBlock :: Vec3 Int -> Engine (Maybe Block)
getBlock pos = do
  EngineState{..} <- ask
  chunks <- liftIO $ get esChunks

  let cdiv = (`divMod` 16) <$> pos
      cpos = fromIntegral . fst <$> cdiv
      Vec3 x y z = snd <$> cdiv
      idx = fromIntegral $ (x * 16 + y) * 16 + z

  case Map.lookup cpos chunks of
    Nothing -> return Nothing
    Just ref -> do
      Chunk{..} <- liftIO $ get ref
      block <- liftIO $ Vector.read chBlocks idx
      return $ case block of
        Empty -> Nothing
        x -> Just x

-- |Places a block on the cursor
placeBlock :: Engine ()
placeBlock = do
  EngineState{..} <- ask
  cursor <- liftIO $ get esCursor
  chunks <- liftIO $ get esChunks

  when (isJust cursor) $ do
    let Just ( pos, dir ) = cursor
        pos' = fromIntegral <$> pos
        dir' = fromIntegral <$> dir
        cdiv = (`divMod` 16) . floor <$> (pos' ^+^ dir')
        cpos = fromIntegral . fst <$> cdiv
        Vec3 x y z = snd <$> cdiv
        idx = fromIntegral $ (x * 16 + y) * 16 + z

    case Map.lookup cpos chunks of
      Nothing -> return ()
      Just ref -> liftIO $ do
        Chunk{..} <- liftIO $ get ref
        Vector.write chBlocks idx Stone
        chDirty $= True

-- |Deletes the block selected by the cursor
deleteBlock :: Engine ()
deleteBlock = do
  EngineState{..} <- ask
  cursor <- liftIO $ get esCursor
  chunks <- liftIO $ get esChunks

  when (isJust cursor) $ do
    let Just ( pos, dir ) = cursor
        cdiv = (`divMod` 16) <$> pos
        cpos = fromIntegral . fst <$> cdiv
        Vec3 x y z = snd <$> cdiv
        idx = fromIntegral $ (x * 16 + y) * 16 + z

    case Map.lookup cpos chunks of
      Nothing -> return ()
      Just ref -> liftIO $ do
        Chunk{..} <- liftIO $ get ref
        Vector.write chBlocks idx Empty
        chDirty $= True

-- |Deletes a chunk
deleteChunk :: Int -> Engine ()
deleteChunk idx
  = return ()