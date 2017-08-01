{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Tuitter (
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

bindir     = "/Users/tatsuki/GitHub/Tuitter/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/bin"
libdir     = "/Users/tatsuki/GitHub/Tuitter/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/lib/x86_64-osx-ghc-8.0.2/Tuitter-0.1.0.0-1eVgorDwDJr5D2XTSKaCrs"
dynlibdir  = "/Users/tatsuki/GitHub/Tuitter/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/tatsuki/GitHub/Tuitter/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/share/x86_64-osx-ghc-8.0.2/Tuitter-0.1.0.0"
libexecdir = "/Users/tatsuki/GitHub/Tuitter/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/libexec"
sysconfdir = "/Users/tatsuki/GitHub/Tuitter/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Tuitter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Tuitter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Tuitter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Tuitter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tuitter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tuitter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
