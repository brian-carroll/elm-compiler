{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Generate.C.JsWrapper
  ( generate
  , mode
  )
  where


import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Name as Name
import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName

import qualified Generate.Mode as Mode
import qualified Generate.C.Literals as CL
import qualified Generate.JavaScript as JS
import qualified Generate.JavaScript.Builder as JSB
import qualified Generate.JavaScript.Name as JSN
import qualified Generate.JavaScript.Functions as JsFunctions


type Mains = Map.Map ModuleName.Canonical Opt.Main


generate :: CL.Literals -> JS.State -> Mains -> B.Builder
generate literals jsState mains =
  "var " <> (JSN.toBuilder wasmWrapperName) <> ";\n"
  <> "(function(scope){\n'use strict';"
  <> defineOnReady
  <> emscriptenPostRun (
      JsFunctions.functions
      <> JS.stateToBuilder jsState
      <> initWrapper literals
      <> assignMains mains
      <> JS.toMainExports mode mains
      <> executeOnReadyCallback
    )
  <> "}(this));"


mode :: Mode.Mode
mode =
  Mode.Dev Nothing



emscriptenModuleRef :: String
emscriptenModuleRef =
  "scope['Module']"


emscriptenPostRun :: B.Builder -> B.Builder
emscriptenPostRun postRunJsCode =
  let
    emscripten = B.stringUtf8 emscriptenModuleRef
  in
  emscripten <> " = " <> emscripten <> " || {};\n"
  <> emscripten <> ".postRun = function postRun() {\n"
  <> postRunJsCode
  <> "}\n"


defineOnReady :: B.Builder
defineOnReady = [r|

var onReadyCallback;
scope['Elm'] = {
  onReady: function(callback) {
    onReadyCallback = callback;
  }
};

|]


executeOnReadyCallback :: B.Builder
executeOnReadyCallback = [r|

if (onReadyCallback) {
  onReadyCallback();
} else {
  throw new Error(`
    Elm.onReady has not been called.
    Elm Wasm apps are initialised differently. You have to initialize your app using a callback function.
    I'll call that function when the WebAssembly |] <> "module" <> [r| is ready.
    It's compiled asynchronously in the browser, so we have to do it this way.
    Your code could look something like this, for example:
       Elm.onReady(() => {
          var app = Elm.Main.init({
             node: document.getElementById('elm'),
             flags: Date.now()
          });
          app.ports.cache.subscribe(function(data) {
            localStorage.setItem('cache', JSON.stringify(data));
          });
       });
  `);
}

|]


wrapWasmElmApp :: String
wrapWasmElmApp =
  "wrapWasmElmApp"


importsFromElm :: [JSN.Name]
importsFromElm =
  [ JSN.fromKernel Name.list "Cons"
  , JSN.fromKernel Name.list "Nil"
  , JSN.fromKernel Name.utils "Tuple0"
  , JSN.fromKernel Name.utils "Tuple2"
  , JSN.fromKernel Name.utils "Tuple3"
  , JSN.fromKernel Name.utils "chr"
  ] ++ map JSN.makeF [2..9]


wasmWrapperName :: JSN.Name
wasmWrapperName =
  JSN.fromLocal $ Name.fromChars "wasmWrapper"


assignMains :: Mains -> B.Builder
assignMains mains =
  let
    (_, builder) =
      Map.foldrWithKey assignMainsHelp (0, "") mains
  in
  builder


assignMainsHelp :: ModuleName.Canonical -> Opt.Main -> (Int, B.Builder) -> (Int, B.Builder)
assignMainsHelp moduleName _ (index, builder) =
  let
    globalName =
      JSN.fromGlobal moduleName (Name.fromChars "main")
    stmt =
      JSB.Var globalName $
      JSB.Index
        (JSB.Access
          (JSB.Ref wasmWrapperName)
          (JSN.fromLocal $ Name.fromChars "mains"))
        (JSB.Int index)
  in
  ( index + 1
  , (JSB.stmtToBuilder stmt) <> builder
  )


initWrapper :: CL.Literals -> B.Builder
initWrapper literals@(CL.Literals _ _ _ _ _ _ fieldGroups ctors kernelJs globalJs) =
  let
    appFields =
      CL.combineFieldLiterals literals
    appFieldGroups =
      Set.toList fieldGroups
    appCtors =
      Set.toList ctors
    appKernelVals = 
      (map (\(home, name) -> JSN.fromKernel home name) (Set.toList kernelJs)) ++
      (map (\(Opt.Global home name) -> JSN.fromGlobal home name) (Set.toList globalJs))

    makeName =
      JSN.fromLocal . Name.fromChars

    wrapperImportObj =
      JSB.Object $ map
        (\n -> (n, JSB.Ref n))
        importsFromElm

    fgStrings = map
      (\fNames ->
        JSB.String $
          mconcat $ List.intersperse " " $
          map Name.toBuilder fNames)
      appFieldGroups

    appTypes =
      JSB.Object
        [ ( makeName "ctors"
          , JSB.Array $ map (JSB.String . Name.toBuilder) appCtors
          )
        , ( makeName "fields"
          , JSB.Array $ map (JSB.String . Name.toBuilder) (Set.toList appFields)
          )
        , ( makeName "fieldGroups"
          , JSB.Array fgStrings
          )
        ]

    emscriptenModule =
      JSB.Ref $ makeName emscriptenModuleRef
    
    kernelRecord =
      JSB.Object $ map
        (\jsName -> (jsName, JSB.Ref jsName))
        appKernelVals
  in
  JSB.stmtToBuilder $ JSB.ExprStmt $
    JSB.Assign (JSB.LRef wasmWrapperName) $
    JSB.Call (JSB.Ref $ makeName wrapWasmElmApp) 
      [ emscriptenModule
      , wrapperImportObj
      , appTypes
      , kernelRecord
      ] 
