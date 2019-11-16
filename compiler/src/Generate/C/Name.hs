{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Name
( Name
, local
, global
, globalInitFn
, globalInitPtr
, jsKernelVal
, cKernelVal
, ctor
, field
, fieldGroup
, evaluator
, literalInt
, literalFloat
, literalString
, literalChar
)
where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)

import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

  
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


cycle :: ModuleName.Canonical -> Name.Name -> Name
cycle home name
  Name $ builderFromUtf8List $ homeToList (home ++ ["cyclic", name])


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


literalInt :: Int -> B.Builder
literalInt x =
  Name $ "literal_int_" <> B.intDec x


literalFloat :: EF.Float -> B.Builder
literalFloat x =
  Name $ "literal_float_" <> (builderFromUtf8List $ Utf8.split dot x)


literalString :: ES.String -> Name
literalString s =
  Name $ "literal_string_" ++ (concatMap escapeChar $ ES.toChars s)


literalChar :: ES.String -> Name
literalChar s =
  Name $ "literal_char_" ++ (concatMap escapeChar $ ES.toChars s)


-- INTERNAL UTILS


escapeChar :: Char -> B.Builder
escapeChar c =
  if Char.isAscii c && (Char.isAlphaNum c || c == '_') then
    B.char8 c
  else
    "\\U" ++ (B.int32HexFixed $ fromIntegral $ Char.ord c)


globalBuilder :: ModuleName.Canonical -> Name.Name -> Name
globalBuilder home name =
  builderFromUtf8List (homeToList home ++ [name])


builderFromUtf8List :: [Utf8.Utf8 t] -> B.Builder
builderFromUtf8List names =
  mconcat $ List.intersperse "_" $
    map Name.toBuilder names


homeToList :: ModuleName.Canonical -> [Name.Name]
homeToList (ModuleName.Canonical (Pkg.Name author project) mod) =
  Utf8.split hyphen author
  ++ Utf8.split hyphen project
  ++ Utf8.split dot mod


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
