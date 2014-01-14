module Main where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Vector.Mutable as Vector
import           Graphics.Rendering.OpenGL
import           HCraft.Craft
import           HCraft.Math
import           HCraft.Engine

main :: IO ()
main = do
  state <- EngineState <$> newIORef True
                       <*> newIORef (Size 800 600)
                       <*> newIORef Map.empty
                       <*> newIORef Map.empty
                       <*> newIORef Map.empty
                       <*> newIORef Nothing
                       <*> newIORef Nothing
                       <*> newIORef Nothing
                       <*> newIORef Nothing
                       <*> newIORef Camera{ cPosition = Vec3 0.0 0.0 0.0
                                          , cRotation = Vec3 0.0 0.0 0.0
                                          , cDirection = Vec3 0.0 0.0 1.0
                                          , cProjMat = mident
                                          , cViewMat = mident
                                          , cSkyMat = mident
                                          , cAspect = 0.0
                                          , cFOV = 45.0
                                          , cNearPlane = 0.1
                                          , cFarPlane = 1000.0
                                          }
                       <*> newIORef Map.empty
                       <*> newIORef Nothing
                       <*> newIORef 0

  status <- runReaderT (runErrorT . run $ engine) state

  case status of
    Right _ -> return ()
    Left err -> putStrLn err