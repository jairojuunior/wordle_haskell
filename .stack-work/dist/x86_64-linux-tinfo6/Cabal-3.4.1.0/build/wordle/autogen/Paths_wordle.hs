{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_wordle (
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

bindir     = "/home/jairo/codes/wordle_haskell/.stack-work/install/x86_64-linux-tinfo6/8546fbdf64eac3615a017de682f5528c5c2904cc831cc9fafbe5ea503f3ebbee/9.0.2/bin"
libdir     = "/home/jairo/codes/wordle_haskell/.stack-work/install/x86_64-linux-tinfo6/8546fbdf64eac3615a017de682f5528c5c2904cc831cc9fafbe5ea503f3ebbee/9.0.2/lib/x86_64-linux-ghc-9.0.2/wordle-0.1.0.0-EPVBPdJ4CW9HNjX4Un18bg-wordle"
dynlibdir  = "/home/jairo/codes/wordle_haskell/.stack-work/install/x86_64-linux-tinfo6/8546fbdf64eac3615a017de682f5528c5c2904cc831cc9fafbe5ea503f3ebbee/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/jairo/codes/wordle_haskell/.stack-work/install/x86_64-linux-tinfo6/8546fbdf64eac3615a017de682f5528c5c2904cc831cc9fafbe5ea503f3ebbee/9.0.2/share/x86_64-linux-ghc-9.0.2/wordle-0.1.0.0"
libexecdir = "/home/jairo/codes/wordle_haskell/.stack-work/install/x86_64-linux-tinfo6/8546fbdf64eac3615a017de682f5528c5c2904cc831cc9fafbe5ea503f3ebbee/9.0.2/libexec/x86_64-linux-ghc-9.0.2/wordle-0.1.0.0"
sysconfdir = "/home/jairo/codes/wordle_haskell/.stack-work/install/x86_64-linux-tinfo6/8546fbdf64eac3615a017de682f5528c5c2904cc831cc9fafbe5ea503f3ebbee/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wordle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wordle_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wordle_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wordle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wordle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wordle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
