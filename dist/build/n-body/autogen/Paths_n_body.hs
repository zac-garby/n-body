{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_n_body (
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

bindir     = "/Users/zacgarby/Library/Haskell/bin"
libdir     = "/Users/zacgarby/Library/Haskell/ghc-8.4.2-x86_64/lib/n-body-0.1.0.0"
dynlibdir  = "/Users/zacgarby/Library/Haskell/ghc-8.4.2-x86_64/lib/x86_64-osx-ghc-8.4.2"
datadir    = "/Users/zacgarby/Library/Haskell/share/ghc-8.4.2-x86_64/n-body-0.1.0.0"
libexecdir = "/Users/zacgarby/Library/Haskell/libexec/x86_64-osx-ghc-8.4.2/n-body-0.1.0.0"
sysconfdir = "/Users/zacgarby/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "n_body_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "n_body_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "n_body_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "n_body_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "n_body_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "n_body_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
