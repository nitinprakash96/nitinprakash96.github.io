{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_thenitinprakash (
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

bindir     = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/d898950be0425c1c5a144d7a176dca9d3bf88f4666c379b8d3454d1cb7b2e1d1/9.0.2/bin"
libdir     = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/d898950be0425c1c5a144d7a176dca9d3bf88f4666c379b8d3454d1cb7b2e1d1/9.0.2/lib/x86_64-osx-ghc-9.0.2/thenitinprakash-0.1.0.0-DFnZokHtgUVLk4Ljuill2T-site"
dynlibdir  = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/d898950be0425c1c5a144d7a176dca9d3bf88f4666c379b8d3454d1cb7b2e1d1/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/d898950be0425c1c5a144d7a176dca9d3bf88f4666c379b8d3454d1cb7b2e1d1/9.0.2/share/x86_64-osx-ghc-9.0.2/thenitinprakash-0.1.0.0"
libexecdir = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/d898950be0425c1c5a144d7a176dca9d3bf88f4666c379b8d3454d1cb7b2e1d1/9.0.2/libexec/x86_64-osx-ghc-9.0.2/thenitinprakash-0.1.0.0"
sysconfdir = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/d898950be0425c1c5a144d7a176dca9d3bf88f4666c379b8d3454d1cb7b2e1d1/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "thenitinprakash_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "thenitinprakash_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "thenitinprakash_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "thenitinprakash_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "thenitinprakash_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "thenitinprakash_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
