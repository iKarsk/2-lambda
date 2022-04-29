{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cswk_program (
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

bindir     = "/Users/krystian/Documents/Warwick/Year1/cs141/coursework2/submission/cswk-program/.stack-work/install/x86_64-osx/38fa9b19e335546ea58b230e3312d6104915dd061d1be84bda7bf7bb4cd91064/8.10.7/bin"
libdir     = "/Users/krystian/Documents/Warwick/Year1/cs141/coursework2/submission/cswk-program/.stack-work/install/x86_64-osx/38fa9b19e335546ea58b230e3312d6104915dd061d1be84bda7bf7bb4cd91064/8.10.7/lib/x86_64-osx-ghc-8.10.7/cswk-program-0.1.0.0-AXLhfXRcJelJ4Wi01zz4gP-cswk-program-exe"
dynlibdir  = "/Users/krystian/Documents/Warwick/Year1/cs141/coursework2/submission/cswk-program/.stack-work/install/x86_64-osx/38fa9b19e335546ea58b230e3312d6104915dd061d1be84bda7bf7bb4cd91064/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/krystian/Documents/Warwick/Year1/cs141/coursework2/submission/cswk-program/.stack-work/install/x86_64-osx/38fa9b19e335546ea58b230e3312d6104915dd061d1be84bda7bf7bb4cd91064/8.10.7/share/x86_64-osx-ghc-8.10.7/cswk-program-0.1.0.0"
libexecdir = "/Users/krystian/Documents/Warwick/Year1/cs141/coursework2/submission/cswk-program/.stack-work/install/x86_64-osx/38fa9b19e335546ea58b230e3312d6104915dd061d1be84bda7bf7bb4cd91064/8.10.7/libexec/x86_64-osx-ghc-8.10.7/cswk-program-0.1.0.0"
sysconfdir = "/Users/krystian/Documents/Warwick/Year1/cs141/coursework2/submission/cswk-program/.stack-work/install/x86_64-osx/38fa9b19e335546ea58b230e3312d6104915dd061d1be84bda7bf7bb4cd91064/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cswk_program_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cswk_program_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cswk_program_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cswk_program_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cswk_program_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cswk_program_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
