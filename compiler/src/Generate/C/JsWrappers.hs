{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Generate.C.JsWrappers
  ( emscriptenModuleRef
  , emscriptenPostRun
  , wrapWasmElmApp
  , defineOnReady
  , executeOnReadyCallback
  , importsFromElm
  )
  where


import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

import qualified Data.Name as Name
import qualified Generate.JavaScript.Name as JsName


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


importsFromElm :: [JsName.Name]
importsFromElm =
  [ JsName.fromKernel Name.list "Cons"
  , JsName.fromKernel Name.list "Nil"
  , JsName.fromKernel Name.utils "Tuple0"
  , JsName.fromKernel Name.utils "Tuple2"
  , JsName.fromKernel Name.utils "Tuple3"
  , JsName.fromKernel Name.utils "chr"
  ] ++ map JsName.makeF [2..9]

