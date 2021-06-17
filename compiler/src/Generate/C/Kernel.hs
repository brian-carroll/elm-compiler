{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Kernel
( shouldGenJsCode
, shouldGenJsEnumId
, shouldGenStruct
, predefinedJsGlobals
, predefinedJsGlobalDeps
, maxClosureArity
, generateClosure
, generateStructDef
, generateHeader
, generateCustomStruct
, HeaderMacro(..)
)
where

import qualified Data.Set as Set
import qualified Data.ByteString.Builder as B
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8

import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as M
import qualified Generate.C.AST as C
import qualified Generate.C.Name as CN
import qualified AST.Optimized as Opt


-- TARGET LANGUAGES: C & JS


shouldGenJsCode :: M.Canonical -> Bool
shouldGenJsCode home
  | home == M.basics      = False
  | home == M.list        = False
  | home == M.string      = False
  | home == M.char        = False
  | home == M.debug       = False
  | home == M.platform    = False
  | home == bitwiseModule = False
  | home == jsArrayModule = False
  | home == testModule    = False
  | otherwise = True


-- Generate JS kernel function IDs for each function?
shouldGenJsEnumId :: Name.Name -> Name.Name -> Bool
shouldGenJsEnumId home name
  | home == Name.fromChars "Json" = (name == Name.fromChars "run")
  | home == Name.utils    = False
  | home == Name.basics   = False
  | home == Name.list     = False
  | home == Name.string   = False
  | home == Name.char     = False
  | home == Name.bitwise  = False
  | home == Name.jsArray  = False
  | home == Name.debug    = False
  | home == Name.platform = False
  | home == "Scheduler"   = False
  | home == "Test"        = False
  | otherwise = True


-- Generate C Closure structs for each function?
shouldGenStruct :: Name.Name -> Name.Name -> Bool
shouldGenStruct home name
  | home == Name.fromChars "Json" = False
  | home == Name.utils    = False
  | home == Name.basics   = False
  | home == Name.list     = False
  | home == Name.string   = False
  | home == Name.char     = False
  | home == Name.bitwise  = False
  | home == Name.jsArray  = False
  | home == Name.debug    = False
  | home == Name.platform = False
  | home == "Scheduler"   = False
  | home == "Test"        = False
  | otherwise = True


predefinedJsGlobals :: [Opt.Global]
predefinedJsGlobals =
  [ Opt.Global (M.Canonical Pkg.kernel Name.platform) Name.dollar
  , Opt.Global (M.Canonical Pkg.kernel "Scheduler") Name.dollar
  , Opt.Global (M.Canonical Pkg.core Name.task) "$fx$"
  ]


-- TODO: figure out why this isn't working... or actually parse the custom kernel JS files
predefinedJsGlobalDeps :: [Opt.Global]
predefinedJsGlobalDeps =
  []
  -- [ Opt.Global (M.Canonical Pkg.kernel "Json") "run"
  -- , Opt.Global (M.Canonical Pkg.kernel "Json") "wrap"
  -- , Opt.Global (M.Canonical Pkg.core Name.result) "isOk"
  -- ]


-- KERNEL MODULES


pkgName :: [Char] -> [Char] -> Pkg.Name
pkgName author project =
  Pkg.Name (Utf8.fromChars author) (Utf8.fromChars project)


bitwiseModule :: M.Canonical
bitwiseModule = M.Canonical Pkg.core Name.bitwise


jsArrayModule :: M.Canonical
jsArrayModule = M.Canonical Pkg.core Name.jsArray


testModule :: M.Canonical
testModule = M.Canonical (pkgName "elm-explorations" "test") "Test"



-- KERNEL DATA STRUCTURES


maxClosureArity :: Int
maxClosureArity =
  0xffff


generateClosure :: CN.Name -> C.Expression -> Int -> [C.Expression] -> C.ExternalDeclaration
generateClosure name evalFnPtr maxValues values =
  let nValues = length values
  in
  generateStructDef CN.Closure name
    [ ("header", generateHeader $ HEADER_CLOSURE nValues)
    , ("n_values", C.Const $ C.IntHexConst nValues)
    , ("max_values", C.Const $ C.IntHexConst maxValues)
    , ("evaluator", evalFnPtr)
    ]
    (if nValues > 0 then Just ("values", values) else Nothing)



generateCustomStruct :: CN.Name -> CN.Name -> C.ExternalDeclaration
generateCustomStruct structName ctorName =
  generateStructDef
    CN.Custom
    structName
    [ ("header", generateHeader $ HEADER_CUSTOM 0)
    , ("ctor", C.Var ctorName)
    ]
    Nothing


generateStructDef :: CN.KernelTypeDef
  -> CN.Name 
  -> [(B.Builder, C.Expression)]
  -> Maybe (B.Builder, [C.Expression])
  -> C.ExternalDeclaration
generateStructDef structName varName fixedMembers flexibleMembers =
  let
    fixed = map
      (\(memberBuilder, memberExpr) ->
        ([C.MemberDesig memberBuilder], C.InitExpr $ memberExpr))
      fixedMembers

    flexible = maybe []
      (\(memberBuilder, memberExprs) ->
        [( [C.MemberDesig memberBuilder]
         , C.InitExpr $ C.CompoundLit $
            map (\expr -> ([], C.InitExpr expr)) memberExprs
         )]
      )
      flexibleMembers
  in
  C.DeclExt $ C.Decl
    [C.TypeSpec $ C.TypeDef structName]
    (Just $ C.Declr (Just $ varName) [])
    (Just $ C.InitExpr $ C.CompoundLit $ (fixed ++ flexible))



-- C VALUE HEADERS


data HeaderMacro
  = HEADER_INT
  | HEADER_FLOAT
  | HEADER_CHAR
  | HEADER_STRING Int
  | HEADER_LIST
  | HEADER_TUPLE2
  | HEADER_TUPLE3
  | HEADER_CUSTOM Int
  | HEADER_RECORD Int
  | HEADER_FIELDGROUP Int
  | HEADER_CLOSURE Int


generateHeader :: HeaderMacro -> C.Expression
generateHeader header =
  let
    fixedSize macro =
      C.Var $ CN.fromBuilder macro
    varSize macro kids =
      C.Call
        (C.Var $ CN.fromBuilder macro)
        [C.Const $ C.IntConst kids]
  in
  case header of
    HEADER_INT -> fixedSize  "HEADER_INT"
    HEADER_FLOAT -> fixedSize "HEADER_FLOAT"
    HEADER_CHAR -> fixedSize "HEADER_CHAR"
    HEADER_STRING n -> varSize "HEADER_STRING" n
    HEADER_LIST -> fixedSize "HEADER_LIST"
    HEADER_TUPLE2 -> fixedSize "HEADER_TUPLE2"
    HEADER_TUPLE3 -> fixedSize "HEADER_TUPLE3"
    HEADER_CUSTOM n -> varSize "HEADER_CUSTOM" n
    HEADER_RECORD n -> varSize "HEADER_RECORD" n
    HEADER_FIELDGROUP n -> varSize "HEADER_FIELDGROUP" n
    HEADER_CLOSURE n -> varSize "HEADER_CLOSURE" n
