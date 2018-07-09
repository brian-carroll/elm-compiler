{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Identifier
  ( fromLocal
  , labelFromLocal
  , fromGlobal
  , fromCycle
  , fromFuncType
  -- , fromIndex
  -- , fromInt

  -- , fromKernel
  -- , makeF
  -- , makeA
  -- , makeLabel
  -- , makeTemp
  -- , dollar
  )
  where


  import Data.Monoid ((<>))

  import qualified AST.Module.Name as ModuleName
  import qualified Generate.JavaScript.Name as JS
  import qualified Elm.Name as N
  import qualified Generate.WebAssembly.AST as WA
  import qualified Generate.WebAssembly.Builder as WAB

  -- CONSTRUCTORS

  -- TODO: eliminate unnecessary work like avoiding JS keywords

  fromLocal :: N.Name -> WA.LocalId
  fromLocal name =
    WA.LocalName $ "$" <> (JS.toBuilder $ JS.fromLocal name)


  labelFromLocal :: N.Name -> WA.LabelId
  labelFromLocal name =
    WA.LabelName $ "$" <> (JS.toBuilder $ JS.fromLocal name)


  fromGlobal :: ModuleName.Canonical -> N.Name -> WA.GlobalId
  fromGlobal moduleName name =
    WA.GlobalName $ "$" <> (JS.toBuilder $ JS.fromGlobal moduleName name)


  fromCycle :: ModuleName.Canonical -> N.Name -> WA.GlobalId
  fromCycle moduleName name =
    WA.GlobalName $ "$" <> (JS.toBuilder $ JS.fromCycle moduleName name)


  fromFuncType :: [WA.ValType] -> WA.ValType -> WA.TypeId
  fromFuncType argTypes resultType =
    WA.TypeName $
      "$funcType$"
        <> (mconcat $ map WAB.buildValType argTypes)
        <> "$"
        <> (WAB.buildValType resultType)

