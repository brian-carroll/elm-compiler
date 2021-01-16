{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Kernel
( shouldGenJsCode
, shouldGenJsEnumId
, shouldGenStruct
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

import qualified Elm.ModuleName as ModuleName
import qualified Generate.C.AST as C
import qualified Generate.C.Name as CN


shouldGenJsCode :: ModuleName.Canonical -> Bool
shouldGenJsCode home
  | home == ModuleName.basics = False
  | home == ModuleName.list   = False
  | home == ModuleName.string = False
  | home == ModuleName.char   = False
  | otherwise = True


shouldGenJsEnumId :: Name.Name -> Name.Name -> Bool
shouldGenJsEnumId home name
  | home == Name.fromChars "Json" = (name == Name.fromChars "run")
  | home == Name.utils  = False
  | home == Name.basics = False
  | home == Name.list   = False
  | home == Name.string = False
  | home == Name.char   = False
  | otherwise = True


shouldGenStruct :: Name.Name -> Name.Name -> Bool
shouldGenStruct home name
  | home == Name.fromChars "Json" = False
  | home == Name.utils  = False
  | home == Name.basics = False
  | home == Name.list   = False
  | home == Name.string = False
  | home == Name.char   = False
  | otherwise = True


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
