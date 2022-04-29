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

bindir     = "/home/joe/2048/2-lambda/.stack-work/install/x86_64-linux-tinfo6/cc58021b69f5cae06b1e4d79d0230d9a2f4e583cea28d8cad0b6cc5c486120b9/8.10.7/bin"
libdir     = "/home/joe/2048/2-lambda/.stack-work/install/x86_64-linux-tinfo6/cc58021b69f5cae06b1e4d79d0230d9a2f4e583cea28d8cad0b6cc5c486120b9/8.10.7/lib/x86_64-linux-ghc-8.10.7/cswk-program-0.1.0.0-KQEWBwkUvHwEJnNJbtWf11"
dynlibdir  = "/home/joe/2048/2-lambda/.stack-work/install/x86_64-linux-tinfo6/cc58021b69f5cae06b1e4d79d0230d9a2f4e583cea28d8cad0b6cc5c486120b9/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/joe/2048/2-lambda/.stack-work/install/x86_64-linux-tinfo6/cc58021b69f5cae06b1e4d79d0230d9a2f4e583cea28d8cad0b6cc5c486120b9/8.10.7/share/x86_64-linux-ghc-8.10.7/cswk-program-0.1.0.0"
libexecdir = "/home/joe/2048/2-lambda/.stack-work/install/x86_64-linux-tinfo6/cc58021b69f5cae06b1e4d79d0230d9a2f4e583cea28d8cad0b6cc5c486120b9/8.10.7/libexec/x86_64-linux-ghc-8.10.7/cswk-program-0.1.0.0"
sysconfdir = "/home/joe/2048/2-lambda/.stack-work/install/x86_64-linux-tinfo6/cc58021b69f5cae06b1e4d79d0230d9a2f4e583cea28d8cad0b6cc5c486120b9/8.10.7/etc"

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
