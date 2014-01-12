module HCraft.Renderer.Mesh.Mesh where

import Graphics.Rendering.OpenGL

-- |Represents an OpenGL mesh source
data MeshSource
  = MeshSource { msVertices :: [ GLfloat ]
               }
  deriving ( Eq, Ord, Show )

-- |Represent and OpenGL mesh description
data MeshDesc
  = MeshDesc { mdName   :: String
             , mdType   :: PrimitiveMode
             , mdSource :: Either String MeshSource
             }
  deriving ( Eq, Ord, Show )

-- |OpenGL mesh
data MeshObject
  = MeshObject { moVAO    :: VertexArrayObject
               , moVBO    :: BufferObject
               , moLength :: GLsizei
               , moType   :: PrimitiveMode
               }
  deriving ( Eq, Ord, Show )

-- |List of builtin meshes
meshBuiltin :: [ MeshDesc ]
meshBuiltin
  = [ -- Individual chunks index this mesh using 16bit index arrays
      -- This mesh is quite large (3Mb), but a lot of bandwidth is
      -- saved by building only index arrays for individual chunks
      MeshDesc "chunkMesh" Triangles (Right . MeshSource . concat $
      [ [ x + 0,  y + 1,  z + 1,  0.0,  0.0,  1.0,  0.0,  1.0
        , x + 0,  y + 0,  z + 1,  0.0,  0.0,  1.0,  0.0,  0.0
        , x + 1,  y + 1,  z + 1,  0.0,  0.0,  1.0,  1.0,  1.0
        , x + 1,  y + 0,  z + 1,  0.0,  0.0,  1.0,  1.0,  0.0

        , x + 0,  y + 0,  z + 0,  0.0,  0.0,  0.0,  0.0,  0.0
        , x + 0,  y + 1,  z + 0,  0.0,  0.0,  0.0,  0.0,  1.0
        , x + 1,  y + 0,  z + 0,  0.0,  0.0,  0.0,  1.0,  0.0
        , x + 1,  y + 1,  z + 0,  0.0,  0.0,  0.0,  1.0,  1.0

        , x + 1,  y + 0,  z + 1,  1.0,  0.0,  0.0,  0.0,  1.0
        , x + 1,  y + 0,  z + 0,  1.0,  0.0,  0.0,  0.0,  0.0
        , x + 1,  y + 1,  z + 1,  1.0,  0.0,  0.0,  1.0,  1.0
        , x + 1,  y + 1,  z + 0,  1.0,  0.0,  0.0,  1.0,  0.0

        , x + 0,  y + 0,  z + 1,  0.0,  0.0,  0.0,  0.0,  1.0
        , x + 0,  y + 1,  z + 1,  0.0,  0.0,  0.0,  1.0,  1.0
        , x + 0,  y + 0,  z + 0,  0.0,  0.0,  0.0,  0.0,  0.0
        , x + 0,  y + 1,  z + 0,  0.0,  0.0,  0.0,  1.0,  0.0

        , x + 0,  y + 0,  z + 1,  0.0,  0.0,  0.0,  0.0,  1.0
        , x + 0,  y + 0,  z + 0,  0.0,  0.0,  0.0,  0.0,  0.0
        , x + 1,  y + 0,  z + 1,  0.0,  0.0,  0.0,  1.0,  1.0
        , x + 1,  y + 0,  z + 0,  0.0,  0.0,  0.0,  1.0,  0.0

        , x + 0,  y + 1,  z + 0,  0.0,  1.0,  0.0,  0.0,  0.0
        , x + 0,  y + 1,  z + 1,  0.0,  1.0,  0.0,  0.0,  1.0
        , x + 1,  y + 1,  z + 0,  0.0,  1.0,  0.0,  1.0,  0.0
        , x + 1,  y + 1,  z + 1,  0.0,  1.0,  0.0,  1.0,  1.0
        ]
      | x <- [0..15], y <- [0..15], z <- [0..15]
      ])
      -- Mesh used by the cursor which selects blocks
    , MeshDesc "cursor" Triangles (Right . MeshSource $
      [ 0.0,  1.0,  1.0,  0.0,  0.0,  1.0,  0.0,  1.0
      , 0.0,  0.0,  1.0,  0.0,  0.0,  1.0,  0.0,  0.0
      , 1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0,  1.0
      , 1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0,  1.0
      , 0.0,  0.0,  1.0,  0.0,  0.0,  1.0,  0.0,  0.0
      , 1.0,  0.0,  1.0,  0.0,  0.0,  1.0,  1.0,  0.0

      , 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0
      , 0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      , 1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0
      , 1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0
      , 0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      , 1.0,  1.0,  0.0,  0.0,  0.0,  0.0,  1.0,  1.0

      , 1.0,  0.0,  1.0,  1.0,  0.0,  0.0,  0.0,  1.0
      , 1.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0
      , 1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0
      , 1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0
      , 1.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0
      , 1.0,  1.0,  0.0,  1.0,  0.0,  0.0,  1.0,  0.0

      , 0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  1.0
      , 0.0,  1.0,  1.0,  0.0,  0.0,  0.0,  1.0,  1.0
      , 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0
      , 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0
      , 0.0,  1.0,  1.0,  0.0,  0.0,  0.0,  1.0,  1.0
      , 0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0

      , 0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  1.0
      , 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0
      , 1.0,  0.0,  1.0,  0.0,  0.0,  0.0,  1.0,  1.0
      , 1.0,  0.0,  1.0,  0.0,  0.0,  0.0,  1.0,  1.0
      , 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0
      , 1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0

      , 0.0,  1.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0
      , 0.0,  1.0,  1.0,  0.0,  1.0,  0.0,  0.0,  1.0
      , 1.0,  1.0,  0.0,  0.0,  1.0,  0.0,  1.0,  0.0
      , 1.0,  1.0,  0.0,  0.0,  1.0,  0.0,  1.0,  0.0
      , 0.0,  1.0,  1.0,  0.0,  1.0,  0.0,  0.0,  1.0
      , 1.0,  1.0,  1.0,  0.0,  1.0,  0.0,  1.0,  1.0
      ])
      -- This mesh is used for occlusion queries, it is a simple cube
    , MeshDesc "chunkOccluder" Triangles (Right $ MeshSource
      [   0.0,  16.0,  16.0,  0.0,  0.0,  1.0,  0.0,  0.0
      ,   0.0,   0.0,  16.0,  0.0,  0.0,  1.0,  0.0,  1.0
      ,  16.0,  16.0,  16.0,  0.0,  0.0,  1.0,  1.0,  0.0
      ,  16.0,  16.0,  16.0,  0.0,  0.0,  1.0,  1.0,  0.0
      ,   0.0,   0.0,  16.0,  0.0,  0.0,  1.0,  0.0,  1.0
      ,  16.0,   0.0,  16.0,  0.0,  0.0,  1.0,  1.0,  1.0

      ,   0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      ,   0.0,  16.0,   0.0,  0.0,  0.0,  0.0,  0.0,  0.0
      ,  16.0,  16.0,   0.0,  0.0,  0.0,  0.0,  1.0,  0.0
      ,   0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      ,  16.0,  16.0,   0.0,  0.0,  0.0,  0.0,  1.0,  0.0
      ,  16.0,   0.0,   0.0,  0.0,  0.0,  0.0,  1.0,  1.0

      ,  16.0,   0.0,  16.0,  1.0,  0.0,  0.0,  0.0,  0.0
      ,  16.0,   0.0,   0.0,  1.0,  0.0,  0.0,  0.0,  1.0
      ,  16.0,  16.0,  16.0,  1.0,  0.0,  0.0,  1.0,  0.0
      ,  16.0,  16.0,  16.0,  1.0,  0.0,  0.0,  1.0,  0.0
      ,  16.0,   0.0,   0.0,  1.0,  0.0,  0.0,  0.0,  1.0
      ,  16.0,  16.0,   0.0,  1.0,  0.0,  0.0,  1.0,  1.0

      ,   0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      ,   0.0,   0.0,  16.0,  0.0,  0.0,  0.0,  0.0,  0.0
      ,   0.0,  16.0,  16.0,  0.0,  0.0,  0.0,  1.0,  0.0
      ,   0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      ,   0.0,  16.0,  16.0,  0.0,  0.0,  0.0,  1.0,  0.0
      ,   0.0,  16.0,   0.0,  0.0,  0.0,  0.0,  1.0,  1.0

      ,   0.0,   0.0,  16.0,  0.0,  0.0,  0.0,  0.0,  0.0
      ,   0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      ,  16.0,   0.0,  16.0,  0.0,  0.0,  0.0,  1.0,  0.0
      ,  16.0,   0.0,  16.0,  0.0,  0.0,  0.0,  1.0,  0.0
      ,   0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  0.0,  1.0
      ,  16.0,   0.0,   0.0,  0.0,  0.0,  0.0,  1.0,  1.0

      ,   0.0,  16.0,   0.0,  0.0,  1.0,  0.0,  0.0,  1.0
      ,   0.0,  16.0,  16.0,  0.0,  1.0,  0.0,  0.0,  0.0
      ,  16.0,  16.0,  16.0,  0.0,  1.0,  0.0,  1.0,  0.0
      ,   0.0,  16.0,   0.0,  0.0,  1.0,  0.0,  0.0,  1.0
      ,  16.0,  16.0,  16.0,  0.0,  1.0,  0.0,  1.0,  0.0
      ,  16.0,  16.0,   0.0,  0.0,  1.0,  0.0,  1.0,  1.0
      ])
      -- An inside-out box used to render the skybox
    , MeshDesc "skybox" Triangles (Right $ MeshSource
      [ -1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  0.0,  0.0
      ,  1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0,  0.0
      , -1.0, -1.0,  1.0,  0.0,  0.0,  1.0,  0.0,  1.0
      ,  1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0,  0.0
      ,  1.0, -1.0,  1.0,  0.0,  0.0,  1.0,  1.0,  1.0
      , -1.0, -1.0,  1.0,  0.0,  0.0,  1.0,  0.0,  1.0

      , -1.0, -1.0, -1.0,  0.0,  0.0, -1.0,  0.0,  1.0
      ,  1.0,  1.0, -1.0,  0.0,  0.0, -1.0,  1.0,  0.0
      , -1.0,  1.0, -1.0,  0.0,  0.0, -1.0,  0.0,  0.0
      , -1.0, -1.0, -1.0,  0.0,  0.0, -1.0,  0.0,  1.0
      ,  1.0, -1.0, -1.0,  0.0,  0.0, -1.0,  1.0,  1.0
      ,  1.0,  1.0, -1.0,  0.0,  0.0, -1.0,  1.0,  0.0

      ,  1.0, -1.0,  1.0,  1.0,  0.0,  0.0,  0.0,  0.0
      ,  1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  0.0
      ,  1.0, -1.0, -1.0,  1.0,  0.0,  0.0,  0.0,  1.0
      ,  1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  0.0
      ,  1.0,  1.0, -1.0,  1.0,  0.0,  0.0,  1.0,  1.0
      ,  1.0, -1.0, -1.0,  1.0,  0.0,  0.0,  0.0,  1.0

      , -1.0, -1.0, -1.0, -1.0,  0.0,  0.0,  0.0,  1.0
      , -1.0,  1.0,  1.0, -1.0,  0.0,  0.0,  1.0,  0.0
      , -1.0, -1.0,  1.0, -1.0,  0.0,  0.0,  0.0,  0.0
      , -1.0, -1.0, -1.0, -1.0,  0.0,  0.0,  0.0,  1.0
      , -1.0,  1.0, -1.0, -1.0,  0.0,  0.0,  1.0,  1.0
      , -1.0,  1.0,  1.0, -1.0,  0.0,  0.0,  1.0,  0.0

      , -1.0, -1.0,  1.0,  0.0, -1.0,  0.0,  0.0,  0.0
      ,  1.0, -1.0,  1.0,  0.0, -1.0,  0.0,  1.0,  0.0
      , -1.0, -1.0, -1.0,  0.0, -1.0,  0.0,  0.0,  1.0
      ,  1.0, -1.0,  1.0,  0.0, -1.0,  0.0,  1.0,  0.0
      ,  1.0, -1.0, -1.0,  0.0, -1.0,  0.0,  1.0,  1.0
      , -1.0, -1.0, -1.0,  0.0, -1.0,  0.0,  0.0,  1.0

      , -1.0,  1.0, -1.0,  0.0,  1.0,  0.0,  0.0,  1.0
      ,  1.0,  1.0,  1.0,  0.0,  1.0,  0.0,  1.0,  0.0
      , -1.0,  1.0,  1.0,  0.0,  1.0,  0.0,  0.0,  0.0
      , -1.0,  1.0, -1.0,  0.0,  1.0,  0.0,  0.0,  1.0
      ,  1.0,  1.0, -1.0,  0.0,  1.0,  0.0,  1.0,  1.0
      ,  1.0,  1.0,  1.0,  0.0,  1.0,  0.0,  1.0,  0.0
      ])
    ]
