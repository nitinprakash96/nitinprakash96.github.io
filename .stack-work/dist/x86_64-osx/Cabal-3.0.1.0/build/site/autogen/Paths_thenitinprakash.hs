{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
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

bindir     = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/07be09702e2a6bad2de37ce591235310ec643f2e28444027bc6d9eced8ee6ac3/8.8.4/bin"
libdir     = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/07be09702e2a6bad2de37ce591235310ec643f2e28444027bc6d9eced8ee6ac3/8.8.4/lib/x86_64-osx-ghc-8.8.4/thenitinprakash-0.1.0.0-72hhZJ4u8qRECTFb9TG15C-site"
dynlibdir  = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/07be09702e2a6bad2de37ce591235310ec643f2e28444027bc6d9eced8ee6ac3/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/07be09702e2a6bad2de37ce591235310ec643f2e28444027bc6d9eced8ee6ac3/8.8.4/share/x86_64-osx-ghc-8.8.4/thenitinprakash-0.1.0.0"
libexecdir = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/07be09702e2a6bad2de37ce591235310ec643f2e28444027bc6d9eced8ee6ac3/8.8.4/libexec/x86_64-osx-ghc-8.8.4/thenitinprakash-0.1.0.0"
sysconfdir = "/Users/supersaiyan/Documents/nitinprakash96.github.io/.stack-work/install/x86_64-osx/07be09702e2a6bad2de37ce591235310ec643f2e28444027bc6d9eced8ee6ac3/8.8.4/etc"

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
