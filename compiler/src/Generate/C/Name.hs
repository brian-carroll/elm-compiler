{-# LANGUAGE OverloadedStrings, EmptyDataDecls, FlexibleInstances #-}
module Generate.C.Name
where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import Data.Char (toLower)
import qualified Data.Set as Set
import qualified Data.String as Chars
import GHC.Word (Word8)
import qualified Language.C as C

import qualified Data.Utf8 as Utf8
import qualified Data.Name as Name
import qualified Generate.C.Builder as CB
import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as ModuleName


type CName =
  Utf8.Utf8 C_NAME


data C_NAME


-- INSTANCES

instance Chars.IsString (Utf8.Utf8 C_NAME) where
  fromString = Utf8.fromChars


-- TO

toChars :: CName -> [Char]
toChars =
  Utf8.toChars


toBuilder :: CName -> B.Builder
toBuilder =
  Utf8.toBuilder


toIdent :: CName -> C.Ident
toIdent name =
  C.mkIdent C.nopos (toChars name) (C.Name 0)


-- FROM


{-# INLINE fromUtf8 #-}
fromUtf8 :: Utf8.Utf8 t -> Utf8.Utf8 C_NAME
fromUtf8 (Utf8.Utf8 name) =
  Utf8.Utf8 name


fromChars :: [Char] -> CName
fromChars =
  Utf8.fromChars


fromLocal :: Name.Name -> CName
fromLocal name =
  fromUtf8 $
    if Set.member name reservedNames
    then Utf8.join underscore [Utf8.empty, name]
    else name


fromGlobal :: ModuleName.Canonical -> Name.Name -> CName
fromGlobal home name =
  Utf8.join underscore $
    (homeToList home) ++ [fromLocal name]


fromCycle :: ModuleName.Canonical -> Name.Name -> CName
fromCycle home name =
  Utf8.join underscore $
    (homeToList home) ++ [fromChars "cyclic", fromLocal name]


fromKernel :: Name.Name -> Name.Name -> CName
fromKernel home name =
  fromUtf8 $ Utf8.join underscore [home, name]


kernelHeaderFile :: ModuleName.Canonical -> CName
kernelHeaderFile (ModuleName.Canonical (Pkg.Name author project) mod) =
  let
    lowercaseChars =
      map (\c -> if c == '.' then '_' else toLower c) (Name.toChars mod)
  in
  fromChars $ lowercaseChars ++ ".h"


homeToList :: ModuleName.Canonical -> [CName]
homeToList (ModuleName.Canonical (Pkg.Name author project) mod) =
  [ replaceChar hyphen underscore author
  , replaceChar hyphen underscore project
  , replaceChar dot underscore mod
  ]


replaceChar :: Word8 -> Word8 -> Utf8.Utf8 t -> CName
replaceChar bad good name =
  Utf8.join good $
    Utf8.split bad $
    fromUtf8 name
    

underscore :: Word8
underscore = 0x5F


hyphen :: Word8
hyphen = 0x2D


dot :: Word8
dot = 0x2E


-- RESERVED NAMES


reservedNames :: Set.Set Name.Name
reservedNames =
  Set.union cReservedWords elmReservedWords


cReservedWords :: Set.Set Name.Name
cReservedWords =
  Set.fromList
    [ "auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern"
    , "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short"
    , "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"
    , "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local"
    , "alignas", "alignof", "bool", "complex", "imaginary", "noreturn", "static_assert", "thread_local"
    , "elif", "endif", "defined", "ifdef", "ifndef", "define", "undef", "include", "line", "error", "pragma"
    , "_Pragma"
    , "asm", "fortran"
    ]


elmReservedWords :: Set.Set Name.Name
elmReservedWords =
  Set.fromList
    [ "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    ]


-- CONSTRUCTOR AND FIELD IDs


asCtor :: CName -> CName
asCtor name =
  Utf8.join underscore [fromChars "Ctor", name]


asField :: CName -> CName
asField name =
  Utf8.join underscore [fromChars "Field", name]


-- KERNEL TYPES


typeFieldGroup :: CName
typeFieldGroup = fromChars "FieldGroup"
