{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Frogger (
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

bindir     = "/Users/jack/.cabal/bin"
libdir     = "/Users/jack/.cabal/lib/x86_64-osx-ghc-8.4.3/Frogger-0.1.0.0-EbIPxn7LYSbDEV4l38JYI8-Frogger"
dynlibdir  = "/Users/jack/.cabal/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/jack/.cabal/share/x86_64-osx-ghc-8.4.3/Frogger-0.1.0.0"
libexecdir = "/Users/jack/.cabal/libexec/x86_64-osx-ghc-8.4.3/Frogger-0.1.0.0"
sysconfdir = "/Users/jack/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Frogger_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Frogger_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Frogger_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Frogger_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Frogger_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Frogger_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
