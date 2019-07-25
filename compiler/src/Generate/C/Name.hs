{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Name
where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import Data.Char (toLower)

import qualified Language.C as C
import qualified Data.Name as Name
import qualified Generate.C.Builder as CB
import qualified Elm.ModuleName as ModuleName


ctorPrefix :: String
ctorPrefix = "Ctor"


fieldPrefix :: String
fieldPrefix = "Field_"


identFromField :: Name.Name -> C.Ident
identFromField field =
  CB.identWithPrefix fieldPrefix field
      

identFromCtor :: Name.Name -> C.Ident
identFromCtor field =
  CB.identWithPrefix ctorPrefix field


idFieldGroup :: C.Ident
idFieldGroup = CB.identFromChars "FieldGroup"


kernelBaseFilename :: ModuleName.Canonical -> B.Builder
kernelBaseFilename (ModuleName.Canonical pkg mod) =
  let
    lowerCaseName = B.stringUtf8 $ map toLower $ Name.toChars mod
  in
  lowerCaseName <> ".h"


kernelIncludeDir :: B.Builder
kernelIncludeDir =
  "../kernel/"
