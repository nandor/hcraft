{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
-- |PNG Image Loader
module HCraft.Renderer.Texture.PNG
  ( readPNG
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL
import HCraft.Engine
import HCraft.Renderer.Texture.Texture

-- |PNG Read struct
newtype PNG
  = PNG { ptrPNG :: Ptr () }
  deriving ( Storable )

-- |PNG Info struct
newtype PNGInfo
  = PNGInfo { ptrPNGInfo :: Ptr () }
  deriving ( Storable )

-- |PNG Error handler
type PNGError
  = FunPtr (PNG -> Ptr CChar -> IO ())

foreign import ccall unsafe "stdio.h fopen"
  fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "stdio.h fread"
  fread :: Ptr a -> CSize -> CSize -> Ptr CFile -> IO CSize

foreign import ccall unsafe "stdio.h fclose"
  fclose :: Ptr CFile -> IO ()

foreign import ccall unsafe "png.h png_sig_cmp"
  pngSigCmp :: Ptr CUChar -> CSize -> CSize -> CInt

foreign import ccall unsafe "png.h png_create_read_struct"
  pngCreateReadStruct :: Ptr CChar -> Ptr a -> PNGError -> PNGError -> IO PNG

foreign import ccall unsafe "png.h png_create_info_struct"
  pngCreateInfoStruct :: PNG -> IO PNGInfo

foreign import ccall unsafe "png.h png_init_io"
  pngInitIO :: PNG -> Ptr CFile -> IO ()

foreign import ccall unsafe "png.h png_set_sig_bytes"
  pngSetSigBytes :: PNG -> CInt -> IO ()

foreign import ccall unsafe "png.h png_read_info"
  pngReadInfo :: PNG -> PNGInfo -> IO ()

foreign import ccall unsafe "png.h png_get_image_width"
  pngGetImageWidth :: PNG -> PNGInfo -> IO CUInt

foreign import ccall unsafe "png.h png_get_image_height"
  pngGetImageHeight :: PNG -> PNGInfo -> IO CUInt

foreign import ccall unsafe "png.h png_get_bit_depth"
  pngGetBitDepth :: PNG -> PNGInfo -> IO CUChar

foreign import ccall unsafe "png.h png_get_rowbytes"
  pngGetRowBytes :: PNG -> PNGInfo -> IO CUInt

foreign import ccall unsafe "png.h png_get_color_type"
  pngGetColorType :: PNG -> PNGInfo -> IO CUChar

foreign import ccall unsafe "png.h png_get_valid"
  pngGetValid :: PNG -> PNGInfo -> CUInt -> IO CUInt

foreign import ccall unsafe "png.h png_set_expand_gray_1_2_4_to_8"
  pngSetExpandGray124to8 :: PNG -> IO ()

foreign import ccall unsafe "png.h png_set_palette_to_rgb"
  pngSetPaletteToRGB :: PNG -> IO ()

foreign import ccall unsafe "png.h png_set_strip_16"
  pngSetStrip16 :: PNG -> IO ()

foreign import ccall unsafe "png.h png_read_update_info"
  pngReadUpdateInfo :: PNG -> PNGInfo -> IO ()

foreign import ccall unsafe "png.h png_read_image"
  pngReadImage :: PNG -> Ptr (Ptr CUChar) -> IO ()

foreign import ccall unsafe "png.h png_destroy_read_struct"
  pngDestroyReadStruct :: Ptr PNG -> Ptr PNGInfo -> Ptr PNGInfo -> IO ()

foreign import ccall unsafe "png.h png_read_end"
  pngReadEnd :: PNG -> PNGInfo -> IO ()

-- |Read a PNG image from a file
readPNG :: FilePath -> Engine TexSource
readPNG path = liftIO $ do
  -- Open the file
  file <- liftIO $ withCString path $ \path' ->
    withCString "rb" $ \mode -> do
      file <- fopen path' mode
      when (file == nullPtr) $
        fail $ "Cannot open file '" ++ path ++ "'"
      return file

  -- Read & check the signature
  withArray [0..8] $ \sig -> do
    len <- fread sig 1 8 file
    when (len /= 8) $ do
      fclose file
      fail "Cannot read signature"

    when (pngSigCmp sig 0 8 /= 0) $ do
      fclose file
      fail "Invalid signature"

  -- Create the png read struct
  png <- withCString "1.2.5" $ \ver ->
    pngCreateReadStruct ver nullPtr nullFunPtr nullFunPtr
  when (ptrPNG png == nullPtr) $ do
     fclose file
     fail "Cannot create PNG read struct"

  info <- pngCreateInfoStruct png
  when (ptrPNGInfo info == nullPtr) $ do
    fclose file
    fail "Cannot create PNG info struct"

  -- Start reading the file
  pngInitIO png file
  pngSetSigBytes png 8
  pngReadInfo png info

  -- Read the header
  width <- fromIntegral <$> pngGetImageWidth png info
  height <- fromIntegral <$> pngGetImageHeight png info
  depth <- pngGetBitDepth png info
  colorType <- pngGetColorType png info

  -- Adjust the file based on color type & guess the texture type
  fmt <- case colorType of
    4 -> return LuminanceAlpha
    6 -> return RGBA
    2 -> return RGB
    0 -> do
      when (depth < 8) $
        pngSetExpandGray124to8 png
      return Luminance
    3 -> do
      pngSetPaletteToRGB png
      return RGB
    _ -> do
      fclose file
      fail $ "Invalid color type: " ++ show colorType

  -- Convert transparency to an alpha channel
  hasTrns <- pngGetValid png info 0x0010
  when (hasTrns /= 0) $
    pngSetStrip16 png

  pngReadUpdateInfo png info
  rowBytes <- fromIntegral <$> pngGetRowBytes png info
  let rowBytes' = rowBytes + 3 - mod (rowBytes - 1) 4

  -- Allocate storage for the image
  ptrData <- mallocArray (rowBytes' * height)
  ptrRows <- newArray . take height . iterate (`advancePtr` rowBytes') $ ptrData

  -- Read the image
  pngReadImage png ptrRows
  pngReadEnd png info
  pixels <- map fromIntegral <$> peekArray (rowBytes' * height) ptrData

  -- Clean up
  with png $ \png' ->
    with info $ \info' ->
      pngDestroyReadStruct png' info' nullPtr

  free ptrData
  free ptrRows
  fclose file

  return $ TexSource (fromIntegral width) (fromIntegral height) fmt pixels
