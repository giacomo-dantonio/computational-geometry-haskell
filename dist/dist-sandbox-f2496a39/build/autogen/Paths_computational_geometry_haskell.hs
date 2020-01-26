{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_computational_geometry_haskell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/giacomo/computational-geometry-haskell/.cabal-sandbox/bin"
libdir     = "/home/giacomo/computational-geometry-haskell/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/computational-geometry-haskell-0.1.0.0-ELowsTfQKqg3KBki8ZDwAu"
dynlibdir  = "/home/giacomo/computational-geometry-haskell/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/giacomo/computational-geometry-haskell/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/computational-geometry-haskell-0.1.0.0"
libexecdir = "/home/giacomo/computational-geometry-haskell/.cabal-sandbox/libexec"
sysconfdir = "/home/giacomo/computational-geometry-haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "computational_geometry_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "computational_geometry_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "computational_geometry_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "computational_geometry_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "computational_geometry_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "computational_geometry_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
