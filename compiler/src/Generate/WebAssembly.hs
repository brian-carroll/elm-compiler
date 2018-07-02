{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly where

import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Monoid ((<>))
import Data.Int (Int32)
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Generate.Mode as Mode
import qualified Generate.WebAssembly.AST as Wasm


type Graph = Map.Map Opt.Global Opt.Node


generateWat :: Mode.Mode -> Map.Map Opt.Global Opt.Node -> Map.Map ModuleName.Canonical Opt.Main -> Builder
generateWat mode graph rootMap =
  let
    state = Map.foldrWithKey (addMain mode graph) emptyState rootMap
  in
    stateToBuilder state


-- GRAPH TRAVERSAL STATE

{-
We don't strictly have to output things in dependency order like JS
But we do need to traverse dependencies in the same way to eliminate dead code
And while we're traversing them, we might as well output them.
-}



data State =
  State
    { _builder :: Builder
    , _startBody :: Builder
    , _table :: Map Wasm.FunctionId Int
    , _dataOffset :: Int32
    , _seenGlobals :: Set.Set Opt.Global  
    }


emptyState :: State
emptyState =
  State
    { _builder = ""
    , _startBody = ""
    , _table = Map.empty
    , _dataOffset = 0
    , _seenGlobals = Set.empty
    }  


stateToBuilder :: State -> Builder
stateToBuilder (State builder startBody table _ _) =
  builder <> startBody <> (generateTable table)


-- Generate code


generateStart :: Builder -> Builder
generateStart body =
  "(func $start\n"
    <> body
    <> ")\n"


generateTable :: Map Wasm.FunctionId Int -> Builder
generateTable table =
  let
    invertedTable =
      Map.foldlWithKey
        (\acc funcId elemIndex ->
            Map.insert elemIndex funcId acc
        )
        Map.empty
        table

    indent = "  "
  in
    "(table anyfunc (elem\n"
      <> (Map.foldlWithKey
            (\acc elemIndex funcId ->
              indent <>
              (case funcId of
                Wasm.FunctionIdx idx -> B.intDec idx
                Wasm.FunctionName name -> name
              )
              <> "\n"
            )
            ""
            invertedTable
          )
      <> "))\n"


addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")


addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph state@(State builder startBody table dataOffset seen) global =
  if Set.member global seen then
    state
  else
    addGlobalHelp mode graph global $
      State builder startBody table dataOffset (Set.insert global seen)


addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
  let
    addDeps deps someState =
      Set.foldl' (addGlobal mode graph) someState deps
  in
    case graph ! global of
      Opt.Define expr deps ->
        -- addStmt (addDeps deps state) (
        --   var global (Expr.generate mode expr)
        -- )
        state
  
      Opt.DefineTailFunc argNames body deps ->
        -- addStmt (addDeps deps state) (
        --   let (Opt.Global _ name) = global in
        --   var global (Expr.generateTailDef mode name argNames body)
        -- )
        state
  
      Opt.Ctor index arity ->
        -- addStmt state (
        --   var global (Expr.generateCtor mode global index arity)
        -- )
        state
  
      Opt.Link linkedGlobal ->
        -- addGlobal mode graph state linkedGlobal
        state
  
      Opt.Cycle names values functions deps ->
        -- addStmt (addDeps deps state) (
        --   generateCycle mode global names values functions
        -- )
        state
  
      Opt.Manager effectsType ->
        -- generateManager mode graph global effectsType state
        state
  
      Opt.Kernel (Opt.KContent clientChunks clientDeps) maybeServer ->
        -- if isDebugger global && not (Mode.isDebug mode) then
        --   state
        -- else
        --   case maybeServer of
        --     Just (Opt.KContent serverChunks serverDeps) | Mode.isServer mode ->
        --       addKernel (addDeps serverDeps state) (generateKernel mode serverChunks)
  
        --     _ ->
        --       addKernel (addDeps clientDeps state) (generateKernel mode clientChunks)
        state
  
      Opt.Enum index ->
        -- addStmt state (
        --   generateEnum mode global index
        -- )
        state
  
      Opt.Box ->
        -- addStmt state (
        --   generateBox mode global
        -- )
        state
  
      Opt.PortIncoming decoder deps ->
        -- addStmt (addDeps deps state) (
        --   generatePort mode global "incomingPort" decoder
        -- )
        state
  
      Opt.PortOutgoing encoder deps ->
        -- addStmt (addDeps deps state) (
        --   generatePort mode global "outgoingPort" encoder
        -- )
        state
  
  