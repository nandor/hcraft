{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module HCraft.Renderer.Texture
  ( module Texture
  , buildTexture
  , bindTexture
  , updateTextures
  ) where

import           Control.Monad hiding (forM_)
import           Control.Monad.Error hiding (forM_)
import           Control.Monad.Reader hiding (forM_)
import qualified Data.Map as Map
import           Data.Foldable (forM_)
import           Foreign hiding (void)
import           Graphics.UI.GLFW
import           Graphics.Rendering.OpenGL
import           HCraft.Engine
import           HCraft.Renderer.Program
import           HCraft.Renderer.Texture.JPEG as Texture
import           HCraft.Renderer.Texture.PNG as Texture
import           HCraft.Renderer.Texture.Texture as Texture

-- |Creates a new texture from from a descriptor
buildTexture :: TexDesc -> Engine ()
buildTexture TexDesc{..} = do
  tex <- liftIO $ genObjectName

  -- Retrieve texture data
  sources <- forM tdSource $ \src -> do
    TexSource{..} <- case src of
          Right src' -> return src'
          Left file -> readPNG file

    if null tsData
      then return ( tsWidth, tsHeight, PixelData tsFmt UnsignedByte nullPtr)
      else do
        arr <- liftIO $ newArray tsData
        return ( tsWidth, tsHeight, PixelData tsFmt UnsignedByte arr)

  -- Retrieve filter parameters
  let ( texFilter, aniso ) = case tdFilter of
       TexNormal -> ( ( ( Nearest, Nothing ), Nearest ), 0.0 )
       TexBilinear -> ( ( ( Linear', Nothing ), Linear' ), 0.0 )
       TexTrilinear -> ( ( ( Linear', Just Linear' ), Linear' ), 0.0 )
       TexAnisotropic x -> ( ( ( Linear', Just Linear' ), Linear' ), x )

  -- Build the texture
  format <- case tdType of
    Tex2D -> liftIO $ do
      textureBinding Texture2D $= Just tex
      textureFilter Texture2D $= texFilter
      textureWrapMode Texture2D S $= ( Repeated, ClampToEdge )
      textureWrapMode Texture2D T $= ( Repeated, ClampToEdge )

      fmts <- forM (zip sources (enumFrom 0)) $ \( ( w, h, pix ), lvl ) -> do
        texImage2D Texture2D NoProxy lvl tdFmt (TextureSize2D w h) 0 pix
        return tdFmt

      when (length fmts <= 0 || any (/= head fmts) fmts) $
        fail $ "Invalid sources '" ++ tdName ++ "'"

      when (aniso > 0.0) $
        textureMaxAnisotropy Texture2D $= aniso

    Tex2DRect -> liftIO $ do
      textureBinding TextureRectangle $= Just tex
      textureFilter TextureRectangle $= texFilter
      textureWrapMode TextureRectangle S $= ( Repeated, ClampToEdge )
      textureWrapMode TextureRectangle T $= ( Repeated, ClampToEdge )

      forM_ (zip sources [0..]) $ \( ( w, h, pix ), i ) ->
        texImage2D TextureRectangle NoProxy i tdFmt (TextureSize2D w h) 0 pix

      when (aniso > 0.0) $
        textureMaxAnisotropy Texture2D $= aniso

    TexCubeMap -> liftIO $ do
      textureBinding TextureCubeMap $= Just tex
      textureFilter TextureCubeMap $= texFilter
      textureWrapMode TextureCubeMap S $= ( Repeated, ClampToEdge )
      textureWrapMode TextureCubeMap T $= ( Repeated, ClampToEdge )

      when (length sources < 6) $
        fail "Not enough images for a cube map"

      let faces = [ TextureCubeMapNegativeX, TextureCubeMapPositiveX
                  , TextureCubeMapNegativeY, TextureCubeMapPositiveY
                  , TextureCubeMapNegativeZ, TextureCubeMapPositiveZ
                  ]

      forM_ (zip sources faces) $ \( ( w, h, pix ), face ) ->
        texImage2D face NoProxy 0 tdFmt (TextureSize2D w h) 0 pix

  -- Free temporary memory
  forM_ sources $ \( _, _, PixelData _ _ ptr) ->
    when (ptr /= nullPtr) $
      liftIO $ free ptr

  -- Add the texture to the cache
  EngineState{ esTextures } <- ask
  liftIO $ get esTextures >>= \cache ->
    let obj = TexObject tex tdType tdFmt tdResize
    in esTextures $= Map.insert tdName obj cache

-- |Binds a texture to the given uniform
bindTexture :: Int -> String -> String -> Engine ()
bindTexture idx name unif = do
  EngineState{..} <- ask

  let unit = TextureUnit (fromIntegral idx)

  liftIO $ get esTextures >>= \x -> case Map.lookup name x of
    Nothing -> fail $ "Texture not found: '" ++ name ++ "'"
    Just TexObject{..} -> do
      activeTexture $= unit

      case toTarget of
        Tex2D      -> textureBinding Texture2D $= Just toHandle
        Tex2DRect  -> textureBinding TextureRectangle $= Just toHandle
        TexCubeMap -> textureBinding TextureCubeMap $= Just toHandle

  parameter unif unit

-- |Resizes all the textures which have the resize flag set
updateTextures :: Engine ()
updateTextures = do
  EngineState{..} <- ask
  liftIO $ do
    osize@(Size nw nh) <- get esSize
    nsize@(Size nw nh) <- get windowSize

    let pixData = PixelData RGBA UnsignedByte nullPtr
        size = TextureSize2D nw nh

    when (osize /= nsize) $
      get esTextures >>= \x -> forM_ x $ \tex@TexObject{..} ->
        when toResize $ case toTarget of
          Tex2D -> do
            textureBinding Texture2D $= Just toHandle
            texImage2D Texture2D NoProxy 0 toFmt size 0 pixData
          Tex2DRect -> do
            textureBinding TextureRectangle $= Just toHandle
            texImage2D TextureRectangle NoProxy 0 toFmt size 0 pixData
          _ ->
            fail "Cannot resize texture"
