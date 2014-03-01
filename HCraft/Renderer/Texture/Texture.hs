module HCraft.Renderer.Texture.Texture where

import Control.Monad.State
import Data.Word
import System.Random
import Graphics.Rendering.OpenGL

-- |List of texture types
data TexTarget
  = Tex2D
  | Tex2DRect
  | Tex2DArray
  | Tex2DShadow
  | TexCubeMap
  | TexCubeMapArray
  | TexBuffer
  deriving ( Eq, Ord, Show )

-- |List of texture filters
data TexFilter
  = TexNormal
  | TexBilinear
  | TexTrilinear
  | TexAnisotropic GLfloat
  deriving ( Eq, Ord, Show )

-- |Describes an image source
data TexSource
  = TexSource { tsWidth  :: GLsizei
              , tsHeight :: GLsizei
              , tsFmt    :: PixelFormat
              , tsData   :: [ Word8 ]
              }
  deriving ( Eq, Ord, Show )

-- |Describes a texture
data TexDesc
  = TexDesc { tdName   :: String
            , tdType   :: TexTarget
            , tdFmt    :: PixelInternalFormat
            , tdResize :: Bool
            , tdFilter :: TexFilter
            , tdSource :: [ Either String TexSource ]
            }
  deriving ( Eq, Ord, Show )

-- |OpenGL texture object
data TexObject
  = TexObject { toHandle :: TextureObject
              , toTarget :: TexTarget
              , toFmt    :: PixelInternalFormat
              , toResize :: Bool
              }
  deriving ( Eq, Ord, Show )

-- |List of builtin texture
textureBuiltin :: [ TexDesc ]
textureBuiltin
  = [ TexDesc "color0" Tex2DRect RGBA' True TexNormal
      [ Right $ TexSource 0 0 RGBA []
      ]
    , TexDesc "color1" Tex2DRect RG16 True TexNormal
      [ Right $ TexSource 0 0 RGBA []
      ]
    , TexDesc "random" Tex2D RGBA' False TexNormal
      [ Right $ TexSource 64 64 RGBA (randomList (64 * 64 * 4))
      ]
    , TexDesc "cursor" Tex2D RGBA' False TexNormal
      [ Left "texture/cursor.png"
      ]
    , TexDesc "sky" TexCubeMap RGBA' False TexBilinear
      [ Left "texture/sky_xn.png"
      , Left "texture/sky_xp.png"
      , Left "texture/sky_yn.png"
      , Left "texture/sky_yp.png"
      , Left "texture/sky_zn.png"
      , Left "texture/sky_zp.png"
      ]
    , TexDesc "blocks" TexCubeMapArray RGBA' False TexBilinear
      [ Left "texture/grass.png"
      , Left "texture/grass.png"
      , Left "texture/grass.png"
      , Left "texture/grass.png"
      , Left "texture/grass.png"
      , Left "texture/grass.png"
      , Left "texture/stone.png"
      , Left "texture/stone.png"
      , Left "texture/stone.png"
      , Left "texture/stone.png"
      , Left "texture/stone.png"
      , Left "texture/stone.png"
      , Left "texture/bedrock.png"
      , Left "texture/bedrock.png"
      , Left "texture/bedrock.png"
      , Left "texture/bedrock.png"
      , Left "texture/bedrock.png"
      , Left "texture/bedrock.png"
      ]
    ]

-- |Generate a random list for a texture
randomList :: Int -> [ Word8 ]
randomList n
  = evalState (replicateM n (state random)) (mkStdGen 0)

