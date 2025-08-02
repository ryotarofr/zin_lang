{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_megaparsec (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "megaparsec"
version :: Version
version = Version [9,7,0] []

synopsis :: String
synopsis = "Monadic parser combinators"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/mrkkrp/megaparsec"
