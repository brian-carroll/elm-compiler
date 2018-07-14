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
  import Generate.WebAssembly.Instructions
  import qualified Elm.Name as N


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
      , _revStartInstr :: [WA.Instr]
      , _tableSize :: Int32
      , _dataOffset :: Int32
      , _seenGlobals :: Set.Set Opt.Global  
      }


  emptyState :: State
  emptyState =
    State
      { _builder = ""
      , _revStartInstr = []
      , _tableSize = 0
      , _dataOffset = 0
      , _seenGlobals = Set.empty
      }


  -- addExpr :: Opt.Expr -> State -> State
  -- addExpr expr state =
  --   let
  --     exprState :: Expr.ExprState
  --     exprState =
  --       Expr.generate expr $
  --         Expr.initState
  --           (fromIntegral $ _dataOffset state)
  --           (fromIntegral $ Map.size $ _table state)
  --           Set.empty
  --   in
  --     state




  stateToBuilder :: State -> Builder
  stateToBuilder (State builder startBody table _ _) =
  --   builder <> startBody -- <> (generateTable table)
    ""


  -- Generate code


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
  addGlobalHelp mode graph global@(Opt.Global moduleName name) state =
    let
      addDeps deps someState =
        Set.foldl' (addGlobal mode graph) someState deps
    in
      case graph ! global of
        Opt.Define expr deps ->
          let
            depState :: State
            depState =
              addDeps deps state
            
            globalId@(WA.GlobalName uniqueId) =
              Identifier.fromGlobal moduleName name

            exprState :: Expr.ExprState
            exprState =
              Expr.generate expr $
                Expr.initState (_dataOffset depState) (_tableSize depState)
            
            (instr, exprBuilder, flushedExprState) =
              Expr.flushState uniqueId exprState
            -- need table size at some point, but that's all
            -- don't need dataOffset
            
            (decl, newRevStart) =
              case instr of
                WA.ConstOp _ _ ->
                  ( WA.Global globalId (WA.GlobalType WA.Immutable WA.I32) instr
                  , (_revStartInstr depState)
                  )
                _ ->
                  ( WA.Global globalId (WA.GlobalType WA.Mutable WA.I32) $
                      i32_const 0
                  , instr : (_revStartInstr depState)
                  )
          in
            depState
              { _builder = (_builder state) <> exprBuilder <> WAB.toBuilder decl
              , _revStartInstr = newRevStart
              -- , _tableSize = flushedExprState
              -- , _dataOffset = flushedExprState
              }
{-

  data State =
    State
      { _builder :: Builder
      , _revStartInstr :: [WA.Instr]
      , _tableSize :: Int32
      , _dataOffset :: Int32
      , _seenGlobals :: Set.Set Opt.Global  
      }

-}
          -- addStmt (addDeps deps state) (
          --   var global (Expr.generate mode expr)
          -- )
          -- set_global (Identifier.fromGlobal moduleName name)
          
          {-
            Need to declare it anyway.
              Create the Declaration, toBuilder it, and append to builder
            If expr is const, we're done
            If not, it's mutable and we add a 'set_global' in 'start'
          -}
          


    
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
    
   