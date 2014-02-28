module HCraft.Renderer.Program.Program where

import Data.HashMap.Strict (HashMap)
import Graphics.Rendering.OpenGL

-- |Program source information
data ProgDesc
  = ProgDesc { pdName :: String
             , pdSources :: [ FilePath ]
             }
  deriving ( Eq, Ord, Show )

-- |Represents an OpenGL program
data ProgObject
  = ProgObject { poHandle   :: Program
               , poUniforms :: HashMap String UniformLocation
               , poAttribs  :: HashMap String AttribLocation
               }
  deriving ( Eq, Show)

-- |List of builtin programs
progBuiltin :: [ ProgDesc ]
progBuiltin
  = [ ProgDesc "sky" [ "shader/sky.vs"
                     , "shader/sky.fs"
                     ]
    , ProgDesc "terrain" [ "shader/terrain.vs"
                         , "shader/terrain.fs"
                         ]
    , ProgDesc "object" [ "shader/object.vs"
                        , "shader/object.fs"
                        ]
    , ProgDesc "depth" [ "shader/depth.vs"
                       , "shader/depth.fs"
                       ]
--  , ProgDesc "dof" [ "shader/dof.cs"
--                   ]
    ]

-- |Builtin attribute locations
progAttribs :: [ ( String, VariableType, AttribLocation ) ]
progAttribs
  = [ ( "in_vertex", FloatVec3, AttribLocation 0 )
    , ( "in_normal", FloatVec3, AttribLocation 1 )
    , ( "in_uv", FloatVec2, AttribLocation 2 )
    , ( "in_tangent", FloatVec3, AttribLocation 3 )
    , ( "in_bitangent", FloatVec3, AttribLocation 4 )
    , ( "in_bweight", FloatVec4, AttribLocation 5 )
    , ( "in_bname", IntVec4, AttribLocation 6 )
    ]

-- |Builtin output locations
progOutputs :: [ ( String, DrawBufferIndex ) ]
progOutputs
  = [ ( "out_color", 0 )
    , ( "out_dfr_color", 0 )
    , ( "out_dfr_normal", 1 )
    ]
