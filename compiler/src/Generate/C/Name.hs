{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Name
  ( Name
  , toBuilder
  , local
  , global
  , globalInitFn
  , globalInitPtr
  , jsKernelValue
  , cKernelValue
  , cycleVar
  , ctorId
  , fieldId
  , fieldGroup
  , globalEvaluator
  , localEvaluator
  , literalInt
  , literalFloat
  , literalString
  , literalChar
  , unit
  , true
  , false
  )
  where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.Char as Char

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES
import qualified Elm.Float as EF

  
-- NAME


newtype Name =
  Name { toBuilder :: B.Builder }


-- CONSTRUCTORS


local :: Name.Name -> Name
local name =
  Name $ "x" <> Name.toBuilder name


global :: ModuleName.Canonical -> Name.Name -> Name
global home name =
  Name (globalBuilder home name)


globalInitFn :: ModuleName.Canonical -> Name.Name -> Name
globalInitFn home name =
  Name ("init_" <> globalBuilder home name)


globalInitPtr :: ModuleName.Canonical -> Name.Name -> Name
globalInitPtr home name =
  Name ("ptr_" <> globalBuilder home name)


globalEvaluator :: ModuleName.Canonical -> Name.Name -> Name
globalEvaluator home name =
  Name ("eval_" <> globalBuilder home name)


localEvaluator :: ModuleName.Canonical -> Name.Name -> Int -> Name
localEvaluator home name index =
  Name ("eval_" <> globalBuilder home name <> "_lambda" <> B.intDec index)


cycleVar :: ModuleName.Canonical -> Name.Name -> Name
cycleVar home name =
  Name $ mconcat $
    (homeToBuilders home)
    ++ ["cyclic", Name.toBuilder name]


jsKernelValue :: Name.Name -> Name.Name -> Name
jsKernelValue home name =
  Name ("JS_" <> builderFromUtf8List [home, name])


cKernelValue :: Name.Name -> Name.Name -> Name
cKernelValue home name =
  Name $ builderFromUtf8List [home, name]


ctorId :: ModuleName.Canonical -> Name.Name -> Name
ctorId _ name =
  Name $ "CTOR_" <> Name.toBuilder name


fieldId :: ModuleName.Canonical -> Name.Name -> Name
fieldId _ name =
  Name $ "FIELD_" <> Name.toBuilder name


fieldGroup :: [Name.Name] -> Name
fieldGroup fields =
  Name $ "fg_" <> builderFromUtf8List fields


literalInt :: Int -> Name
literalInt x =
  Name $ "literal_int_" <> B.intDec x


literalFloat :: EF.Float -> Name
literalFloat x =
  Name $ "literal_float_" <> (builderFromUtf8List $ Utf8.split dot x)


literalString :: ES.String -> Name
literalString s =
  Name $ mconcat $
    "literal_string_" : (map escapeChar $ ES.toChars s)


literalChar :: ES.String -> Name
literalChar s =
  Name $ mconcat $
    "literal_char_" : (map escapeChar $ ES.toChars s)


-- INTERNAL UTILS


escapeChar :: Char -> B.Builder
escapeChar c =
  if Char.isAscii c then
    if Char.isAlphaNum c then
      B.char8 c
    else
      B.char8 '_'
  else
    "\\U" <> (B.int32HexFixed $ fromIntegral $ Char.ord c)


globalBuilder :: ModuleName.Canonical -> Name.Name -> B.Builder
globalBuilder home name =
  mconcat (homeToBuilders home ++ [Name.toBuilder name])


builderFromUtf8List :: [Utf8.Utf8 t] -> B.Builder
builderFromUtf8List names =
  mconcat $ List.intersperse "_" $
    map Utf8.toBuilder names


-- these Utf8 mofos all have different phantom types
-- need to convert toBuilder first then shove into a list
homeToBuilders :: ModuleName.Canonical -> [B.Builder]
homeToBuilders (ModuleName.Canonical (Pkg.Name author project) modul) =
  [ builderFromUtf8List $ Utf8.split hyphen author
  , builderFromUtf8List $ Utf8.split hyphen project
  , builderFromUtf8List $ Utf8.split dot modul
  ]


hyphen :: Word8
hyphen = 0x2D


dot :: Word8
dot = 0x2E


-- KERNEL CONSTANTS

unit :: Name
unit = Name "Unit"

true :: Name
true = Name "True"

false :: Name
false = Name "False"
