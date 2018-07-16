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
  import qualified Generate.WebAssembly.AST as WA
  import qualified Generate.WebAssembly.Expression as Expr
  import qualified Generate.WebAssembly.Identifier as Identifier
  import qualified Generate.WebAssembly.Builder as WAB
  import qualified Generate.WebAssembly.Test as Test
  import Generate.WebAssembly.Instructions
  import qualified Elm.Name as N


  import qualified Generate.JavaScript.Name as DebugN
  import Debug.Trace as Debug


  type Graph = Map.Map Opt.Global Opt.Node


  generateWat :: Mode.Mode -> Map.Map Opt.Global Opt.Node -> Map.Map ModuleName.Canonical Opt.Main -> Builder
  generateWat mode graph rootMap =
    let
      -- state = Map.foldrWithKey (addMain mode graph) emptyState rootMap
      state = Map.foldrWithKey (addMain mode Test.graph) emptyState Test.rootMap
    in
      stateToBuilder state


  -- GRAPH TRAVERSAL STATE

  data State =
    State
      { _revDecl :: [WA.Declaration]
      , _revStartInstr :: [WA.Instr]
      , _exprState :: Expr.ExprState
      , _seenGlobals :: Set.Set Opt.Global  
      }


  emptyState :: State
  emptyState =
    State
      { _revDecl = []
      , _revStartInstr = []
      , _exprState = Expr.initState 0 0
      , _seenGlobals = Set.empty
      }


  stateToBuilder :: State -> Builder
  stateToBuilder state =
    WAB.buildModule $ generateModule state
        

  -- Generate code


  addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> main -> State -> State
  addMain mode graph home _ state =
    addGlobal mode graph state (Opt.Global home "main")


  addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
  addGlobal mode graph state@(State builder revStart exprState seen) global =
    if Set.member global seen then
      state
    else
      addGlobalHelp mode graph global $
        State builder revStart exprState (Set.insert global seen)


  addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
  addGlobalHelp mode graph global@(Opt.Global moduleName name) state =
    let
      addDeps deps someState =
        Set.foldl' (addGlobal mode graph) someState deps
      
      (Opt.Global moduleName name) = global

      tracedGlobal =
        Debug.trace ("global: " ++ 
          (show $ B.toLazyByteString $ DebugN.toBuilder $ 
            DebugN.fromGlobal moduleName name
          ))
          global

      -- tracedGraph =
      --   Debug.trace ("graph: " ++ show graph) graph
    in
      case graph ! tracedGlobal of
        Opt.Define expr deps ->
          let
            depState =
              addDeps deps state
            
            gid@(WA.GlobalName uniqueId) =
              Identifier.fromGlobal moduleName name

            (instr, exprDecls, flushedExprState) =
              Expr.flushState uniqueId $ Expr.generate expr (_exprState depState)

            (globalDecl, newRevStart) =
              case instr of
                WA.ConstOp _ _ ->
                  ( WA.Global gid WA.Immutable WA.I32 instr
                  , (_revStartInstr depState)
                  )
                _ ->
                  ( WA.Global gid WA.Mutable WA.I32 $ i32_const 0
                  , instr : (_revStartInstr depState)
                  )
          in
            depState
              { _revDecl = globalDecl : exprDecls ++ (_revDecl depState)
              , _revStartInstr = newRevStart
              , _exprState = flushedExprState
              }
    
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



  generateModule :: State -> WA.Module
  generateModule (State revDecl revStart exprState _) =
    let
      startId = WA.FunctionName "$start"
      startFuncDecl =
        WA.Function startId [] [] Nothing (reverse revStart)
    in
      WA.Module
        []
        (Just $ Expr.generateMemory exprState)
        (Just $ Expr.generateTable exprState)
        (Just startId)
        (reverse $ startFuncDecl : revDecl)
