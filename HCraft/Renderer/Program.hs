{-# LANGUAGE NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
module HCraft.Renderer.Program
  ( module HCraft.Renderer.Program.Program
  , buildProgram
  , bindProgram
  , parameter
  , parameterv
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List
import           Foreign
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           System.FilePath
import           HCraft.Engine
import           HCraft.Math
import           HCraft.Renderer.Program.Program

-- | Compiles a shader file
compile :: FilePath -> Engine Shader
compile file = do
  shader <- liftIO $ case takeExtension file of
    ".vs" -> createShader VertexShader
    ".fs" -> createShader FragmentShader
    ".gs" -> createShader GeometryShader
    ".tc" -> createShader TessControlShader
    ".te" -> createShader TessEvaluationShader
    ".cs" -> createShader ComputeShader
    ext -> fail $ "Invalid shader type: '" ++ ext ++ "'"

  status <- liftIO $ do
    source <- BS.readFile file

    shaderSourceBS shader $= if BS.null source then "" else source

    compileShader shader
    get $ compileStatus shader

  unless status $ do
    log <- liftIO . get $ shaderInfoLog shader
    fail $ "(" ++ file ++ ") error: " ++ log

  return shader

-- | Links multiple shader files into a program
buildProgram :: ProgDesc -> Engine ()
buildProgram ProgDesc{..} = do
  prog <- liftIO createProgram
  shaders <- mapM compile pdSources

  -- Link the shader
  status <- liftIO $ do
    forM_ shaders $ \shader ->
      attachShader prog shader

    forM_ progAttribs $ \( name, _, loc ) ->
      attribLocation prog name $= loc

    forM_ progOutputs $ \( name, loc ) ->
      bindFragDataLocation prog name $= loc

    linkProgram prog
    get $ linkStatus prog

  -- Report possible errors
  unless status $ do
    log <- liftIO . get $ programInfoLog prog
    fail $ "(" ++ intercalate "," pdSources ++ ") error: " ++ log

  -- Check uniforms
  unifs <- liftIO $ do
    unifs' <- get $ activeUniforms prog
    forM unifs' $ \( _, _, name ) -> do
      loc <- get $ uniformLocation prog name
      return ( name, loc )

  -- Check attributes
  attribs <- liftIO $ do
    attribs' <- get $ activeAttribs prog
    forM attribs' $ \( _, t, name ) -> do
      loc <- get $ attribLocation prog name
      case find (\( x, _, _ ) -> x == name) progAttribs of
        Nothing -> return ( name, loc)
        Just ( _, t', loc' ) -> do
          when (t /= t') $
            fail $ "Invalid attribute type '" ++ name ++ "'"
          when (loc /= loc') $
            fail $ "Invalid attribute location '" ++ name ++ "'"
          return ( name, loc )

  -- Put shader into cache
  EngineState{..} <- ask
  liftIO $ get esPrograms >>= \cache ->
    let obj = ProgObject prog (Map.fromList unifs) (Map.fromList attribs)
    in esPrograms $= Map.insert pdName obj cache

-- | Binds a program to the OpenGL state
bindProgram :: String -> Engine ()
bindProgram name = do
  EngineState{..} <- ask
  liftIO $ get esPrograms >>= \x -> case Map.lookup name x of
    Nothing -> fail $ "Program not found: '" ++ name ++ "'"
    Just prog@ProgObject{..} -> do
      currentProgram $= Just poHandle
      esProgram $= Just prog


-- | Sets the value of an uniform
parameter :: Uniform a => String -> a -> Engine ()
parameter name val = do
  EngineState{..} <- ask
  liftIO $ get esProgram >>= \x -> case x of
    Nothing -> fail "No program bound"
    Just ProgObject{..}  ->
      case Map.lookup name poUniforms of
        Nothing -> return ()
        Just u ->
          uniform u $= val

-- | Sets the value of a uniform matrix
parameterv :: String -> Mat4 GLfloat -> Engine ()
parameterv name val = do
  EngineState {..} <- ask
  liftIO $ get esProgram >>= \x -> case x of
    Nothing -> fail "No program bound"
    Just ProgObject{..}  ->
      case Map.lookup name poUniforms of
        Nothing -> return ()
        Just (UniformLocation u) ->
          withArray (lfromm val) $ \ptr ->
            glUniformMatrix4fv u 1 0 ptr
