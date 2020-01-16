{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Name
  ( Name
  , fromBuilder
  , toBuilder
  , local
  , tmp
  , label
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
  , globalTailEvaluator
  , localTailEvaluator
  , literalInt
  , literalFloat
  , literalStr
  , literalChr
  , accessor
  , nil
  , unit
  , true
  , false
  , utilsInitGlobal
  , utilsAccessEval
  , utilsUpdate
  , utilsDestructIndex
  , utilsEqual
  , nullPtr
  , appFieldGroups
  , wrapperRegisterFieldGroups
  , mains
  , wrapperRegisterMains
  , gcTceData
  , gcTceEval
  , gcTceIteration
  , canThrowMacro
  , tceLabel
  , KernelTypeDef(..)
  , HeaderFile(..)
  , args
  , applyMacro
  , utilsListFromArray
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
  if name == Name.dollar then  -- JSON encoder for outgoing port
    Name "x"
  else
    Name $ "x_" <> Name.toBuilder name


tmp :: Int -> Name
tmp index =
  Name $ "tmp" <> B.intDec index


label :: Name.Name -> Int -> Name
label prefix index =
  Name $ (Name.toBuilder prefix) <> "_" <> (B.intDec index)


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


globalTailEvaluator :: ModuleName.Canonical -> Name.Name -> Name
globalTailEvaluator home name =
  Name ("tce_" <> globalBuilder home name)


localTailEvaluator :: ModuleName.Canonical -> Name.Name -> Name.Name -> Name
localTailEvaluator home gName name =
  Name ("tce_" <> globalBuilder home gName <> "_" <> Name.toBuilder name)


cycleVar :: ModuleName.Canonical -> Name.Name -> Name
cycleVar = globalInitFn


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


utilsInitGlobal :: Name
utilsInitGlobal =
  kernelValue Name.utils (Name.fromChars "initGlobal")


utilsAccessEval :: Name
utilsAccessEval =
  kernelValue Name.utils (Name.fromChars "access_eval")


utilsUpdate :: Name
utilsUpdate =
  kernelValue Name.utils (Name.fromChars "update")


utilsListFromArray :: Name
utilsListFromArray =
  kernelValue Name.list (Name.fromChars "fromArray")


utilsDestructIndex :: Name
utilsDestructIndex =
  kernelValue Name.utils (Name.fromChars "destruct_index")


utilsEqual :: Name
utilsEqual =
  kernelValue Name.utils (Name.fromChars "equal")
      

nullPtr :: Name
nullPtr =
  Name "NULL"


appFieldGroups :: Name
appFieldGroups =
  Name "app_field_groups"


wrapperRegisterFieldGroups :: Name
wrapperRegisterFieldGroups =
  Name "Wrapper_registerFieldGroups"


mains :: Name
mains =
  Name "mains"


wrapperRegisterMains :: Name
wrapperRegisterMains =
  Name "Wrapper_registerMains"


args :: Name
args =
  Name "args"


tceLabel :: Name
tceLabel =
  Name "tce_loop"

gcTceData :: Name
gcTceData =
  Name "gc_tce_data"


gcTceEval :: Name
gcTceEval =
  Name "GC_tce_eval"


gcTceIteration :: Name
gcTceIteration =
  Name "GC_tce_iteration"


canThrowMacro :: Name
canThrowMacro =
  Name "CAN_THROW"


applyMacro :: Int -> Name
applyMacro n =
  Name $ "A" <> B.intDec n

 
-- C KERNEL TYPE DEFINITIONS

data KernelTypeDef
  = ElmValue
  | ElmInt
  | ElmFloat
  | ElmChar
  | ElmString
  | ElmString16
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
