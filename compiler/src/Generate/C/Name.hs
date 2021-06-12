{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Name
  ( Name
  , fromBuilder
  , toBuilder
  , fromSmallIndex
  , local
  , tmp
  , label
  , global
  , globalInitFn
  , globalInitPtr
  , jsKernelEval
  , jsGlobalEval
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
  , nil
  , unit
  , true
  , false
  , listCreate
  , gcInitRoot
  , utilsAccessEval
  , utilsUpdate
  , utilsEqual
  , nullPtr
  , jsonRun
  , jsonRunEvalIndex
  , wrapperFieldGroups
  , wrapperMains
  , tceLabel
  , KernelTypeDef(..)
  , HeaderFile(..)
  , args
  , applyMacro
  , wasmIntercept
  , managerId
  , createManagerFn
  )
  where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Index as Index

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES
import qualified Elm.Float as EF
import qualified AST.Optimized as Opt

  
-- NAME


newtype Name =
  Name { toBuilder :: B.Builder }


-- CONSTRUCTORS

fromBuilder :: B.Builder -> Name
fromBuilder = Name


fromSmallIndex :: Index.ZeroBased -> Name
fromSmallIndex index =
  Name $ B.char8 $ Char.chr $ (Char.ord 'a') + (Index.toMachine index)


local :: Name.Name -> Name
local name =
  if name == Name.dollar then  -- JSON encoder for outgoing port
    Name "x"
  else
    Name $ "x_" <> Name.toBuilder name


tmp :: B.Builder -> Int -> Name
tmp prefix index =
  Name $ prefix <> B.intDec index


label :: Name.Name -> Int -> Name
label prefix index =
  Name $ (Name.toBuilder prefix) <> "_" <> (B.intDec index)


global :: ModuleName.Canonical -> Name.Name -> Name
global home name =
  Name ("g_" <> globalBuilder home name)


globalInitFn :: ModuleName.Canonical -> Name.Name -> Name
globalInitFn home name =
  Name ("ginit_" <> globalBuilder home name)


globalInitPtr :: ModuleName.Canonical -> Name.Name -> Name
globalInitPtr home name =
  Name ("gptr_" <> globalBuilder home name)


globalEvaluator :: ModuleName.Canonical -> Name.Name -> Name
globalEvaluator home name =
  Name ("eval_" <> globalBuilder home name)


localEvaluator :: ModuleName.Canonical -> Name.Name -> Int -> Name
localEvaluator home name index =
  Name ("eval_" <> globalBuilder home name <> "_lambda" <> B.intDec index)


cycleVar :: ModuleName.Canonical -> Name.Name -> Name
cycleVar = globalInitFn


jsGlobalEval :: ModuleName.Canonical -> Name.Name -> Name
jsGlobalEval home name =
  Name ("JS_" <> globalBuilder home name)


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
  if x >= 0 then
    Name $ "int_" <> B.intDec x
  else
    Name $ "int__" <> B.intDec (-x)


literalFloat :: EF.Float -> Name
literalFloat x =
  Name $ "float_" <>
    (join $ map Utf8.toBuilder $
      concatMap (Utf8.split hyphen) $
      Utf8.split dot x)


literalStr :: ES.String -> Name
literalStr s =
  Name $ mconcat $
    "str_" : (map escapeChar $ ES.toChars s)


literalChr :: ES.String -> Name
literalChr s =
  Name $ mconcat $
    "char_" : (map escapeChar $ ES.toChars s)


accessor :: Name.Name -> Name
accessor name =
  Name $ "access_" <> Name.toBuilder name


managerId :: ModuleName.Canonical -> Name
managerId (ModuleName.Canonical _ moduleName) =
  Name $ "MANAGER_" <> (join $ splitUtf8 dot moduleName)


createManagerFn :: ModuleName.Canonical -> Name
createManagerFn (ModuleName.Canonical _ moduleName) =
  Name $ "createManager_" <> (join $ splitUtf8 dot moduleName)


-- INTERNAL UTILS


escapeChar :: Char -> B.Builder
escapeChar c =
  if Char.isAscii c && Char.isAlphaNum c then
    B.char8 c
  else if c == '_' then
    "__"
  else
    let
      codepoint = Char.ord c
      encoder =
        if codepoint <= 0xff then
          B.word8HexFixed . fromIntegral
        else if codepoint <= 0xffff then      
          B.word16HexFixed . fromIntegral
        else
          B.int32HexFixed . fromIntegral
    in
      "_" <> (encoder codepoint)


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

nil :: Name
nil = Name "Nil"

unit :: Name
unit = Name "Unit"

true :: Name
true = Name "True"

false :: Name
false = Name "False"


listCreate :: Name
listCreate =
  kernelValue Name.list (Name.fromChars "create")


gcInitRoot :: Name
gcInitRoot =
  Name "GC_init_root"


utilsAccessEval :: Name
utilsAccessEval =
  kernelValue Name.utils (Name.fromChars "access_eval")


utilsUpdate :: Name
utilsUpdate =
  kernelValue Name.utils (Name.fromChars "update")


utilsEqual :: Name
utilsEqual =
  kernelValue Name.utils (Name.fromChars "equal")
      

nullPtr :: Name
nullPtr =
  Name "NULL"


jsonRunEvalIndex :: Name
jsonRunEvalIndex =
  Name "Json_run_eval_index"


jsonRun :: Name
jsonRun =
  Name "Json_run"


wrapperFieldGroups :: Name
wrapperFieldGroups =
  Name "Wrapper_appFieldGroups"


wrapperMains :: Name
wrapperMains =
  Name "Wrapper_mainsArray"


args :: Name
args =
  Name "args"


tceLabel :: Name
tceLabel =
  Name "tce_loop"


applyMacro :: Int -> Name
applyMacro n =
  Name $ "A" <> B.intDec n


wasmIntercept :: Opt.Global
wasmIntercept =
  Opt.Global
    (ModuleName.Canonical
      Pkg.dummyName
      (Name.fromChars "WebAssembly"))
    (Name.fromChars "intercept")


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
  | U32
  | F64


data HeaderFile
  = KernelH
