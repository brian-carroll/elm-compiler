{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Name
  ( Name
  , fromBuilder
  , toBuilder
  , local
  , global
  , globalInitFn
  , globalInitPtr
  , jsKernelEval
  , kernelValue
  , cycleVar
  , ctorId
  , fieldId
  , fieldGroup
  , globalEvaluator
  , localEvaluator
  , literalInt
  , literalFloat
  , literalStr
  , literalChr
  , accessor
  , unit
  , true
  , false
  , utilsInitGlobal
  , utilsAccessEval
  , appFieldGroups
  , nullPtr
  , wrapperRegisterFieldGroups
  , wrapperRegisterMainRecord
  , KernelTypeDef(..)
  , HeaderFile(..)
  , args
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

fromBuilder :: B.Builder -> Name
fromBuilder = Name


local :: Name.Name -> Name
local name =
  Name $ "x_" <> Name.toBuilder name


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


jsKernelEval :: Name.Name -> Name.Name -> Name
jsKernelEval home name =
  Name ("JS_" <> (join $ map Name.toBuilder [home, name]))


kernelValue :: Name.Name -> Name.Name -> Name
kernelValue home name =
  Name $ join $ map Name.toBuilder [home, name]


ctorId :: Name.Name -> Name
ctorId name =
  Name $ "CTOR_" <> Name.toBuilder name


fieldId :: Name.Name -> Name
fieldId name =
  Name $ "FIELD_" <> Name.toBuilder name


fieldGroup :: [Name.Name] -> Name
fieldGroup fields =
  Name $ "fg_" <> (join $ map Name.toBuilder fields)


literalInt :: Int -> Name
literalInt x =
  Name $ "literal_int_" <> B.intDec x


literalFloat :: EF.Float -> Name
literalFloat x =
  Name $ "literal_float_" <> (join $ splitUtf8 dot x)


literalStr :: ES.String -> Name
literalStr s =
  Name $ mconcat $
    "literal_string_" : (map escapeChar $ ES.toChars s)


literalChr :: ES.String -> Name
literalChr s =
  Name $ mconcat $
    "literal_char_" : (map escapeChar $ ES.toChars s)


accessor :: Name.Name -> Name
accessor name =
  Name $ "accessor_" <> Name.toBuilder name


-- INTERNAL UTILS


escapeChar :: Char -> B.Builder
escapeChar c =
  if Char.isAscii c && (Char.isAlphaNum c || c == '_') then
    B.char8 c
  else
    let codepoint = Char.ord c in
    if codepoint <= 0xffff then      
      "\\u" <> (B.word16HexFixed $ fromIntegral $ codepoint)
    else
      "\\U" <> (B.int32HexFixed $ fromIntegral $ codepoint)


globalBuilder :: ModuleName.Canonical -> Name.Name -> B.Builder
globalBuilder home name =
  join (homeToBuilders home ++ [Name.toBuilder name])


homeToBuilders :: ModuleName.Canonical -> [B.Builder]
homeToBuilders (ModuleName.Canonical (Pkg.Name author project) modul) =
  (splitUtf8 hyphen author)
  ++ (splitUtf8 hyphen project)
  ++ (splitUtf8 dot modul)


splitUtf8 :: Word8 -> Utf8.Utf8 t -> [B.Builder]
splitUtf8 chr utf8 =
  map Utf8.toBuilder $ Utf8.split chr utf8


join :: [B.Builder] -> B.Builder
join builders =
  mconcat $ List.intersperse "_" builders


hyphen :: Word8
hyphen = 0x2D


dot :: Word8
dot = 0x2E


-- C KERNEL VALUES

unit :: Name
unit = Name "Unit"

true :: Name
true = Name "True"

false :: Name
false = Name "False"


utilsInitGlobal :: Name
utilsInitGlobal =
  kernelValue Name.utils (Name.fromChars "initGlobal")


utilsAccessEval :: Name
utilsAccessEval =
  kernelValue Name.utils (Name.fromChars "access_eval")


appFieldGroups :: Name
appFieldGroups =
  Name "app_field_groups"


nullPtr :: Name
nullPtr =
  Name "NULL"


wrapperRegisterFieldGroups :: Name
wrapperRegisterFieldGroups =
  Name "Wrapper_registerFieldGroups"


wrapperRegisterMainRecord :: Name
wrapperRegisterMainRecord =
  Name "Wrapper_registerMainRecord"


args :: Name
args =
  Name "args"

      
-- C KERNEL TYPE DEFINITIONS

data KernelTypeDef
  = ElmValue
  | ElmInt
  | ElmFloat
  | ElmChar
  | ElmString
  | Cons
  | Tuple2
  | Tuple3
  | Custom
  | Record
  | FieldGroup
  | Closure
  | I32
  | F64


data HeaderFile
  = KernelH
