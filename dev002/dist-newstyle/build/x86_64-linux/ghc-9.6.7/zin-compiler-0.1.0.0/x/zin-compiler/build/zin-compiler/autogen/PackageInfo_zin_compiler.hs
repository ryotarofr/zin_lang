{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_zin_compiler (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "zin_compiler"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A compiler for the zin markup language"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/zin-lang/zin-compiler"
