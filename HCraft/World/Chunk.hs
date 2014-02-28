{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.World.Chunk
  ( module Chunk
  , neighbours
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
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Debug.Trace
import           Foreign
import           Graphics.Rendering.OpenGL
import           System.Random
import           HCraft.World.Chunk.Chunk as Chunk
import           HCraft.World.Chunk.Noise as Chunk
import           HCraft.Engine
import           HCraft.Math
import           HCraft.Renderer.Program
import           HCraft.Renderer.Mesh
import           HCraft.Renderer.Texture

-- |Array containing the coordinates of all the cubes
coords :: (Enum a, Num a) => [ Vec3 a ]
coords
  = let range = [0..chunkSize - 1]
    in [ Vec3 x y z | x <- range, y <- range, z <- range]

-- |Index of neighbours
neighbours :: Num a => Vec3 a -> [ Vec3 a ]
neighbours (Vec3 x y z)
  = [ Vec3 (x + 0) (y + 0) (z + 1)
    , Vec3 (x + 0) (y + 0) (z - 1)
    , Vec3 (x + 1) (y + 0) (z + 0)
    , Vec3 (x - 1) (y + 0) (z + 0)
    , Vec3 (x + 0) (y - 1) (z + 0)
    , Vec3 (x + 0) (y + 1) (z + 0)
    ]

-- |Creates a new chunk or retrieves it from the cache
getChunk :: Vec3 GLint -> Engine (Maybe Chunk)
getChunk pos@(Vec3 cx cy cz) = do
  EngineState{..} <- ask
  chunks <- liftIO $ get esChunks

  liftIO $ case HashMap.lookup (fromIntegral <$> pos) chunks of
    Nothing -> do
      count <- get esCount
      if count < 2
        then do
          esCount $~! (+1)

          -- The new chunk does not have a VAO or query object attached yet
          -- They are built when the chunk is first rendered. Lazy initialization
          -- is used because when building meshes for chunks data from neighbours
          -- might be required, but we do not want their meshes to be built yet
          vec <- Vector.new (chunkSize * chunkSize * chunkSize)
          tex <- genObjectName

          -- Fill in the chunk with random data
          vec' <- forM coords $ \(Vec3 x y z) -> do
            let block = noiseGetBlock (cx * chunkSize + x) (cy * chunkSize + y) (cz * chunkSize + z)
                idx = fromIntegral $ ((x * chunkSize) + y) * chunkSize + z
            liftIO $ Vector.write vec idx block
            return block

          -- Create the texture
          textureBinding Texture3D $= Just tex
          textureFilter Texture3D $= ( ( Nearest, Nothing ), Nearest )
          textureWrapMode Texture3D S $= ( Repeated, ClampToEdge )
          textureWrapMode Texture3D T $= ( Repeated, ClampToEdge )
          withArray vec' $ \ptr -> do
            let pxdata = PixelData RedInteger UnsignedInt ptr
            texImage3D Texture3D NoProxy 0 R32UI (TextureSize3D chunkSize chunkSize chunkSize) 0 pxdata

          -- Add the chunk to the cache
          chunk <- Chunk <$> newIORef Nothing
                         <*> newIORef Nothing
                         <*> newIORef True
                         <*> pure vec
                         <*> pure tex
                         <*> pure pos
                         <*> pure (mat4Trans $ (*chunkSize) . fromIntegral <$> pos)

          ref <- newIORef chunk
          esChunks $= HashMap.insert (fromIntegral <$> pos) ref chunks
          return (Just chunk)
        else
          return Nothing
    Just x -> Just <$> get x

-- |Builds the mesh for a chunk
buildChunk :: Chunk -> Engine (IOVector GLuint, GLsizeiptr )
buildChunk Chunk{..} = liftIO $ do
  let Vec3 cx cy cz = (*chunkSize) <$> chPosition
      maxSize = (chunkSize ^ 3) * 36
      foldM' coords counter func = foldM func counter coords

  arr <- Vector.new maxSize

  len <- {-# SCC "chunk_loop" #-} liftIO $
    foldM' coords 0 $ \len (Vec3 x y z ) -> do -- coord of each block in the chunk
    let idx = (x * chunkSize + y) * chunkSize + z
        pos = Vec3 (cx + x) (cy + y) (cz + z)

        inChunk (Vec3 x y z)
          = cx <= x && x < cx + chunkSize &&
            cy <= y && y < cy + chunkSize &&
            cz <= z && z < cz + chunkSize

    -- Check whether the block is visible
    block <- Vector.unsafeRead chBlocks (fromIntegral idx)
    case block of
      0 -> return len
      _ -> do
        -- Cull faces if neighbours occlude them
        len' <- {-# SCC "neighbour_loop" #-}
          foldM' (zip (neighbours pos) [0..5]) len $ \len ( pos' , f ) -> do
          block <-
            if inChunk pos'
              then let Vec3 x' y' z' = pos'
                       idx' = ((x' - cx) * chunkSize + (y' - cy)) * chunkSize + (z' - cz)
                   in do
                    block <- Vector.unsafeRead chBlocks (fromIntegral idx')
                    return (if block == 0 then Nothing else Just block)
              else do
                return Nothing

          let mesh = fromIntegral $ (idx * 6 + f) * 4

          case block of
            Just _ -> do
              return len
            Nothing -> do
              Vector.unsafeWrite arr (len + 0) (mesh + 0)
              Vector.unsafeWrite arr (len + 1) (mesh + 1)
              Vector.unsafeWrite arr (len + 2) (mesh + 2)
              Vector.unsafeWrite arr (len + 3) (mesh + 2)
              Vector.unsafeWrite arr (len + 4) (mesh + 1)
              Vector.unsafeWrite arr (len + 5) (mesh + 3)
              return $ len + 6
        return len'

  return ( arr, fromIntegral len )

-- |Renders a chunk
renderChunk :: Chunk -> Engine ()
renderChunk chunk@Chunk{..} = do
  EngineState{..} <- ask

  -- If a chunk intersects the camera, the occlusion query is disabled
  inCamera <- liftIO (get esCamera) >>= \Camera{..} ->
    let pos = (fromIntegral . floor . (/chunkSize) <$> cPosition)
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
    parameter "u_blocks" (TextureUnit 1)

    liftIO $ do
      activeTexture $= (TextureUnit 1)
      textureBinding Texture3D $= Just chBlockTex

    case mesh of
      Just ChunkMesh{..} -> liftIO $ do
        bindVertexArrayObject $= Just cmVAO
        drawElements Triangles (fromIntegral cmLength) UnsignedInt nullPtr
        endConditionalRender
      Nothing -> do
        vao <- liftIO genObjectName
        ibo <- liftIO genObjectName

        -- Vertex data
        liftIO $ do
          bindVertexArrayObject $= Just vao
          get esMeshes >>= \cache -> case HashMap.lookup "chunkMesh" cache of
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

        ( indices, len ) <- buildChunk chunk

        -- Index data
        liftIO $ do
          bindBuffer ElementArrayBuffer $= Just ibo

          Vector.unsafeWith indices $ \ptr -> do
            bufferData ElementArrayBuffer $= ( len * 4, ptr, StaticDraw )

          chMesh $= Just (ChunkMesh vao ibo (fromIntegral len))
          drawElements Triangles (fromIntegral len) UnsignedInt nullPtr
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
{-# INLINE getBlock #-}
getBlock :: Vec3 GLint -> Engine (Maybe Int)
getBlock pos = do
  EngineState{..} <- ask
  chunks <- liftIO $ get esChunks

  let cdiv = (`divMod` chunkSize) <$> pos
      cpos = fromIntegral . fst <$> cdiv
      Vec3 x y z = snd <$> cdiv
      idx = fromIntegral $ (x * chunkSize + y) * chunkSize + z

  case HashMap.lookup cpos chunks of
    Nothing -> return Nothing
    Just ref -> do
      Chunk{..} <- liftIO $ get ref
      block <- liftIO $ Vector.read chBlocks idx
      return $ case block of
        0 -> Nothing
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
        cdiv = (`divMod` chunkSize) . floor <$> (pos' ^+^ dir')
        cpos = fromIntegral . fst <$> cdiv
        Vec3 x y z = snd <$> cdiv
        idx = fromIntegral $ (x * chunkSize + y) * chunkSize + z
        tpos = TexturePosition3D z y x
        tsize = TextureSize3D 1 1 1

    case HashMap.lookup cpos chunks of
      Nothing -> return ()
      Just ref -> liftIO $ do
        Chunk{..} <- liftIO $ get ref
        Vector.write chBlocks idx 2

        textureBinding Texture3D $= Just chBlockTex
        withArray [2 :: Int ] $ \ptr -> do
          let pxData = (PixelData RedInteger UnsignedInt ptr)
          texSubImage3D Texture3D 0 tpos tsize pxData

        chDirty $= True

-- |Deletes the block selected by the cursor
deleteBlock :: Engine ()
deleteBlock = do
  EngineState{..} <- ask
  cursor <- liftIO $ get esCursor
  chunks <- liftIO $ get esChunks

  when (isJust cursor) $ do
    let Just ( pos, dir ) = cursor
        cdiv = (`divMod` chunkSize) <$> pos
        cpos = fromIntegral . fst <$> cdiv
        Vec3 x y z = fromIntegral . snd <$> cdiv
        idx = fromIntegral $ (x * chunkSize + y) * chunkSize + z
        tsize = TextureSize3D 1 1 1
        tpos = TexturePosition3D z y x

    case HashMap.lookup cpos chunks of
      Nothing -> return ()
      Just ref -> liftIO $ do
        Chunk{..} <- liftIO $ get ref
        Vector.write chBlocks idx 0

        textureBinding Texture3D $= Just chBlockTex
        withArray [0 :: Int] $ \ptr -> do
          let pxData = (PixelData RedInteger UnsignedInt ptr)
          texSubImage3D Texture3D 0 tpos tsize pxData

        chDirty $= True

-- |Deletes a chunk
deleteChunk :: Int -> Engine ()
deleteChunk idx
  = return ()