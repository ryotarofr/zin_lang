{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_megaparsec (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [9,7,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ryotarofr/.cabal/store/ghc-9.6.7/megaparsec-9.7.0-6601780833244ae49a38561c09eff6ae103727b4a469518386c40c700142a1c1/bin"
libdir     = "/home/ryotarofr/.cabal/store/ghc-9.6.7/megaparsec-9.7.0-6601780833244ae49a38561c09eff6ae103727b4a469518386c40c700142a1c1/lib"
dynlibdir  = "/home/ryotarofr/.cabal/store/ghc-9.6.7/megaparsec-9.7.0-6601780833244ae49a38561c09eff6ae103727b4a469518386c40c700142a1c1/lib"
datadir    = "/home/ryotarofr/.cabal/store/ghc-9.6.7/megaparsec-9.7.0-6601780833244ae49a38561c09eff6ae103727b4a469518386c40c700142a1c1/share"
libexecdir = "/home/ryotarofr/.cabal/store/ghc-9.6.7/megaparsec-9.7.0-6601780833244ae49a38561c09eff6ae103727b4a469518386c40c700142a1c1/libexec"
sysconfdir = "/home/ryotarofr/.cabal/store/ghc-9.6.7/megaparsec-9.7.0-6601780833244ae49a38561c09eff6ae103727b4a469518386c40c700142a1c1/etc"

getBinDir     = catchIO (getEnv "megaparsec_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "megaparsec_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "megaparsec_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "megaparsec_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "megaparsec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "megaparsec_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
