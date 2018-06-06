{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly where

import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Generate.Mode as Mode


generateWast :: Mode.Mode -> Map.Map Opt.Global Opt.Node -> Map.Map ModuleName.Canonical Opt.Main -> B.Builder
generateWast mode graph rootMap =
  "Hello Wasm"
