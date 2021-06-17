{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where

import Prelude hiding (cycle, print)
import qualified Control.Monad.State as StateMonad

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8

import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CN
import qualified Generate.C.Expression as CE
import qualified Generate.C.AST as C
import qualified Generate.C.JsWrapper as JsWrapper
import qualified Generate.C.Kernel as CK
import qualified Generate.C.Literals as CL

import qualified Generate.JavaScript as JS

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName

import Debug.Trace as Debug


-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main


-- GRAPH TRAVERSAL STATE

data State =
  State
    { _seenGlobals :: Set.Set Opt.Global
    , _literals :: CL.Literals
    , _revInitGlobals :: [Opt.Global]
    , _revExtDecls :: [C.ExternalDeclaration]
    , _jsState :: JS.State
    }


generate :: Opt.GlobalGraph -> Mains -> (B.Builder, B.Builder)
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    state = Map.foldrWithKey (addMain graph) (initState graph) mains
    cBuilder = buildC mains state
    jsBuilder = JsWrapper.generate (_literals state) (_jsState state) mains
  in
    (cBuilder, jsBuilder)


initState :: Graph -> State
initState graph =
  let
    jsPredefState = List.foldl' JS.insertSeenGlobal JS.emptyState CK.predefinedJsGlobals
    jsInitState = List.foldl' (JS.addGlobal "" JsWrapper.mode graph)
                    jsPredefState CK.predefinedJsGlobalDeps
  in
  State
    { _seenGlobals = Set.empty
    , _literals = CL.insertKernelJs ("Json", "run") CL.empty
    , _revInitGlobals = []
    , _revExtDecls = []
    , _jsState = jsInitState
    }


{-----------------------------------------------------------

    C FILE

-----------------------------------------------------------}

buildC :: Mains -> State -> B.Builder
buildC mains state =
  prependExtDecls [C.IncludeExt CN.KernelH] $
  prependExtDecls (CL.generate $ _literals state) $
  prependExtDecls (_revExtDecls state) $
  prependExtDecls [generateFunctionDebugNames (_revExtDecls state)] $
  prependExtDecls [generateMainsArray mains, C.BlankLineExt] $
  prependExtDecls [jsonRunIndexAssignment] $
  prependExtDecls [generateCMain state, C.BlankLineExt]
    ""


prependExtDecls :: [C.ExternalDeclaration] -> B.Builder -> B.Builder
prependExtDecls revExtDecls monolith =
  List.foldl' (\m ext -> (CB.fromExtDecl ext) <> m) monolith revExtDecls


{-----------------------------------------------------------

    SHARED DEFINITIONS

-----------------------------------------------------------}


jsonRunIndexAssignment :: C.ExternalDeclaration
jsonRunIndexAssignment =
  C.DeclExt $ C.Decl
    [C.TypeSpec C.SizeT]
    (Just $ C.Declr (Just CN.jsonRunEvalIndex) [])
    (Just $ C.InitExpr $ C.Var $ CN.jsKernelEval "Json" "run")


{-----------------------------------------------------------

    C PROGRAM INITIALISATION

-----------------------------------------------------------}

generateCMain :: State -> C.ExternalDeclaration
generateCMain (State _ literals revInitGlobals _ _) =
  let
    exitCode =
      CN.fromBuilder "exit_code"
    initGC =
      C.BlockDecl $ C.Decl [C.TypeSpec C.Int]
        (Just $ C.Declr (Just exitCode) [])
        (Just $ C.InitExpr $ C.Call (C.Var $ CN.fromBuilder "GC_init") [])
    returnFail =
      C.BlockStmt $ C.If (C.Var exitCode)
        (C.Return $ Just $ C.Var exitCode) Nothing
    initCalls =
      List.foldl' generateInitCall [] revInitGlobals
    initEffectManagers =
      CL.generateInitEffectManagers literals
    runGC =
      C.BlockStmt $ C.Expr $ Just $
      C.Call (C.Var $ CN.fromBuilder "GC_collect_major")
      []
    returnSuccess =
      C.BlockStmt $ C.Return $ Just $ C.Const (C.IntConst 0)
    body =
      [ initGC
      , returnFail
      ] ++
      initCalls ++
      initEffectManagers ++
      [ runGC
      , returnSuccess
      ]
  in
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Int]
    (C.Declr (Just $ CN.fromBuilder "EMSCRIPTEN_KEEPALIVE main") [C.FunDeclr []]) $
    (List.reverse body)


generateMainsArray :: Mains -> C.ExternalDeclaration
generateMainsArray mains =
  let
    initList =
      Map.foldrWithKey
        generateMainsArrayHelp
        [([], C.InitExpr $ C.Var CN.nullPtr)]
        mains
  in
  C.DeclExt $ C.Decl
    [C.TypeSpec C.Void]
    (Just $ C.Declr (Just CN.wrapperMains)
      [C.PtrDeclr [], C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
    (Just $ C.InitExpr $ C.CompoundLit initList)


generateMainsArrayHelp :: ModuleName.Canonical -> Opt.Main -> C.InitializerList -> C.InitializerList
generateMainsArrayHelp moduleName _ arrayElements =
  ([], (C.InitExpr $ C.addrOf $ CN.globalInitPtr moduleName "main"))
  : arrayElements


generateInitCall :: [C.CompoundBlockItem] -> Opt.Global -> [C.CompoundBlockItem]
generateInitCall acc (Opt.Global home name) =
  let
    initCall = C.BlockStmt $ C.Expr $ Just $
      C.Call (C.Var CN.gcInitRoot)
      [ C.Unary C.AddrOp $ C.Var $ CN.globalInitPtr home name
      , C.Unary C.AddrOp $ C.Var $ CN.globalInitFn home name
      ]
  in
  initCall : acc




{-----------------------------------------------------------

                ELM 'MAIN' VALUES

-----------------------------------------------------------}

addMain :: Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain graph home _ state =
  let
    main = Opt.Global home "main"
    stateWithJsFlags = addJsFlagsDecoder graph state main
  in
  addGlobal (Debug.trace "" "") graph stateWithJsFlags main 


addJsFlagsDecoder :: Graph -> State -> Opt.Global -> State
addJsFlagsDecoder graph state main =
  let
    decoderGlobals =
      Set.filter (\(Opt.Global home _) -> home == ModuleName.jsonDecode) $
      case (graph ! main) of
        (Opt.Define _ deps) -> deps
        _ -> Set.empty
  in
  state {
    _jsState =
      Set.foldl'
        (JS.addGlobal "" JsWrapper.mode graph)
        (_jsState state)
        decoderGlobals
    }


addGlobal :: B.Builder -> Graph -> State -> Opt.Global -> State
addGlobal debugIndent graph state global =
  let
    seen = _seenGlobals state
  in
  if Set.member global seen then
    state
  else
    addGlobalHelp debugIndent graph global $
      state
        { _seenGlobals = Set.insert global seen
        , _revExtDecls = C.BlankLineExt : _revExtDecls state
        }


addGlobalHelp :: B.Builder -> Graph -> Opt.Global -> State -> State
addGlobalHelp debugIndentHere graph global state =
  let
    debugIndent =
      debugIndentHere <> "  "
    addDeps deps someState =
      Set.foldl' (addGlobal debugIndent graph) someState $
        -- traceDeps debugIndentHere node global $
        deps
    node =
      graph ! global
  in
  case node of
    Opt.Define expr deps ->
      addDef global expr $
      addDeps deps state

    Opt.DefineTailFunc argNames body deps ->
      addExtDecl (C.CommentExt $ nodeName node) $
      addDeps deps state

    Opt.Ctor _ arity ->
      generateCtor global arity state

    Opt.Link linkedGlobal ->
      addGlobal debugIndent graph state linkedGlobal

    Opt.Cycle names values functions deps ->
      generateCycle global names values functions $
      addDeps deps state

    Opt.Manager effectsType ->
      generateManager debugIndent graph global effectsType state

    Opt.Kernel chunks deps ->
      let
        (Opt.Global home@(ModuleName.Canonical _ moduleName) _) = global
        depState =
          if moduleName == Name.debugger then
            state -- haven't written debugger dependencies in C yet!
          else
            addDeps deps state
      in
      if CK.shouldGenJsCode home then
        depState { _jsState =
          JS.addGlobal debugIndent JsWrapper.mode graph (_jsState state) global
        }
      else
        depState

    Opt.Enum _ ->
      generateCtor global 0 state

    Opt.Box ->
      generateCtor global 1 state

    Opt.PortIncoming decoder deps ->
      addLiteral CL.insertGlobalJs global $
      addDeps deps $
      state { _jsState =
        JS.addGlobal debugIndent JsWrapper.mode graph (_jsState state) global
      }

    Opt.PortOutgoing encoder deps ->
      addLiteral CL.insertGlobalJs global $
      addDeps deps $
      state { _jsState =
        JS.addGlobal debugIndent JsWrapper.mode graph (_jsState state) global
      }


addExtDecl :: C.ExternalDeclaration -> State -> State
addExtDecl extDecl state =
  state { _revExtDecls = extDecl : _revExtDecls state }


addLiteral :: (a -> CL.Literals -> CL.Literals) -> a -> State -> State
addLiteral insert value state =
  state
    { _literals = insert value (_literals state) }


-- Interface to Generate.C.Expression
generateExpression :: Opt.Global -> CN.Name -> StateMonad.State CE.ExprState a -> State -> State
generateExpression global fname exprGenerator state =
  let
    (revExtDecls, literals) =
      CE.globalDefsFromExprState $
        StateMonad.execState exprGenerator $
        CE.initState global fname (_revExtDecls state) (_literals state)
  in
  state
    { _revExtDecls = revExtDecls
    , _literals = literals
    }


{-----------------------------------------------------------

                CYCLE

-----------------------------------------------------------}

generateCycle :: Opt.Global -> [Name.Name] -> [(Name.Name, Opt.Expr)] -> [Opt.Def] -> State -> State
generateCycle (Opt.Global home _) names values functions prevState =
  generateCycleValues home values $
    generateCycleFunctions home functions $
    generateCyclePreDeclClosures home functions $
    generateCyclePreDeclValues home values $
    prevState


generateCyclePreDeclValues :: ModuleName.Canonical -> [(Name.Name, Opt.Expr)] -> State -> State
generateCyclePreDeclValues home values prevState =
  List.foldl'
    (\state (name, _) ->
      addExtDecl
        (C.DeclExt $ C.Decl
          [C.TypeSpec C.Void]
          (Just $ C.Declr
            (Just $ CN.cycleVar home name)
            [C.PtrDeclr [], C.FunDeclr []])
          Nothing
        )
        state
    )
    prevState
    values


generateCyclePreDeclClosures :: ModuleName.Canonical -> [Opt.Def] -> State -> State
generateCyclePreDeclClosures home functions prevState =
  List.foldl'
    (\state def ->
      let
        closureName =
          case def of
            Opt.Def name _ -> CN.global home name
            Opt.TailDef name _ _ -> CN.global home name
      in
      addExtDecl
        (C.DeclExt $ C.Decl
          [C.TypeSpec $ C.TypeDef CN.Closure]
          (Just $ C.Declr (Just closureName) [])
          Nothing
        )
        state
    )
    prevState
    functions


generateCycleFunctions :: ModuleName.Canonical -> [Opt.Def] -> State -> State
generateCycleFunctions home functions prevState =
  List.foldl'
    (\state def ->
      case def of
        Opt.Def name body ->
          addDef (Opt.Global home name) body state

        Opt.TailDef name args body ->
          generateCycleTailDef home name args body state
    )
    prevState
    functions


generateCycleTailDef :: ModuleName.Canonical -> Name.Name -> [Name.Name] -> Opt.Expr -> State -> State
generateCycleTailDef home name args body state =
  let
    global = Opt.Global home name
    fname = CN.globalEvaluator home name
    closureName = CN.global home name

    initExprState =
      CE.initState global fname (_revExtDecls state) (_literals state)

    (revExtDecls, literals) =
      CE.globalDefsFromExprState $
      StateMonad.execState
        (CE.generateEvalFunction fname args body True)
        initExprState

    closure =
      CK.generateClosure closureName (C.Var fname) (length args) []
  in
  state
    { _revExtDecls = closure : revExtDecls
    , _literals = literals
    }


generateCycleValues :: ModuleName.Canonical -> [(Name.Name, Opt.Expr)] -> State -> State
generateCycleValues home values prevState =
  List.foldl' (generateCycleVal home) prevState values


generateCycleVal :: ModuleName.Canonical -> State -> (Name.Name, Opt.Expr) -> State
generateCycleVal home state (name, expr) =
  let
    global = Opt.Global home name
    globalName = CN.global home name
    ptrName = CN.globalInitPtr home name
    funcName = CN.cycleVar home name

    declarePtr =
      C.DeclExt $ C.Decl
        [C.TypeSpec $ C.TypeDef CN.Closure]
        (Just $ C.Declr (Just ptrName) [C.PtrDeclr []])
        Nothing

    defineAlias =
      C.DefineExt globalName $ C.Parens $
      C.Unary C.DerefOp $ C.Var ptrName

    exprGenerator = CE.generateCycleFn ptrName funcName expr
  in
  generateExpression global funcName exprGenerator $
    state
      { _revInitGlobals = global : (_revInitGlobals state)
      , _revExtDecls = declarePtr : defineAlias : _revExtDecls state
      }


{-----------------------------------------------------------

                EFFECT MANAGER

-----------------------------------------------------------}

generateManager :: B.Builder -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
generateManager debugIndent graph global@(Opt.Global home _) effectsType state =
  let
    (deps, args, leaves) =
      generateManagerHelp home effectsType

    createManager =
      C.functionWithoutArgs (CN.createManagerFn home) [] $
        C.Call (C.Var $ CN.fromBuilder "Platform_createManager") args

    jsState = state { _jsState =
               JS.generateManagerWasm debugIndent JsWrapper.mode graph
                  global effectsType (_jsState state) }

    depsState =
      List.foldl' (addGlobal debugIndent graph) jsState deps
  in
  addLiteral CL.insertManager home $
  foldr addExtDecl depsState (createManager : leaves)


generateManagerHelp :: ModuleName.Canonical -> Opt.EffectsType -> ([Opt.Global], [C.Expression], [C.ExternalDeclaration])
generateManagerHelp home effectsType =
  let
    dep name = Opt.Global home name
    ref name = C.addrOf (CN.global home name)
  in
  case effectsType of
    Opt.Cmd ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap", C.Var CN.nullPtr ]
      , [ generateLeaf home "command" ]
      )

    Opt.Sub ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "subMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", C.Var CN.nullPtr, ref "subMap" ]
      , [ generateLeaf home "subscription" ]
      )

    Opt.Fx ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap", dep "subMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap", ref "subMap" ]
      , [ generateLeaf home "command"
        , generateLeaf home "subscription"
        ]
      )


generateLeaf :: ModuleName.Canonical -> Name.Name -> C.ExternalDeclaration
generateLeaf home name =
  CK.generateClosure
    (CN.global home name)
    (C.Var $ CN.fromBuilder "eval_Platform_leaf")
    2
    [C.nameAsVoidPtr $ CN.managerId home]


{-----------------------------------------------------------

                GLOBAL DEFINITION

-----------------------------------------------------------}


addDef :: Opt.Global -> Opt.Expr -> State -> State
addDef global@(Opt.Global home' name') expr state =
  let
    globalName =
      CN.global home' name'

    defineAlias alias state =
      addExtDecl (C.DefineExt globalName $ C.Var alias) state
  in
  case expr of
    Opt.Function args body ->
      if global == CN.wasmIntercept then
        state
      else
        generateGlobalFunction global args body state

    Opt.Int value ->
      addLiteral CL.insertInt value $
        defineAlias (CN.literalInt value) state

    Opt.Float value ->
      addLiteral CL.insertFloat value $
        defineAlias (CN.literalFloat value) state
  
    Opt.Chr value ->
      addLiteral CL.insertChr value $
        defineAlias (CN.literalChr value) state

    Opt.Str value ->
      addLiteral CL.insertStr value $
        defineAlias (CN.literalStr value) state

    Opt.Bool bool ->
      defineAlias (if bool then CN.true else CN.false) state

    Opt.Unit ->
      defineAlias CN.unit state

    Opt.Accessor name ->
      addLiteral CL.insertFieldAccessor name $
        defineAlias (CN.accessor name) state

    Opt.List _        -> generateRuntimeInit CN.Cons global expr state
    Opt.Call _ _      -> generateRuntimeInit CN.Closure global expr state
    Opt.If _ _        -> generateRuntimeInit CN.Closure global expr state
    Opt.Let _ _       -> generateRuntimeInit CN.Closure global expr state
    Opt.Destruct _ _  -> generateRuntimeInit CN.Closure global expr state
    Opt.Case _ _ _ _  -> generateRuntimeInit CN.Closure global expr state
    Opt.Access _ _    -> generateRuntimeInit CN.Closure global expr state
    Opt.Record _      -> generateRuntimeInit CN.Record global expr state
    Opt.Update _ _    -> generateRuntimeInit CN.Record global expr state
    Opt.Shader _ _ _  -> generateRuntimeInit CN.Closure global expr state
    Opt.Tuple _ _ Nothing  -> generateRuntimeInit CN.Tuple2 global expr state
    Opt.Tuple _ _ (Just _) -> generateRuntimeInit CN.Tuple3 global expr state

    Opt.VarGlobal (Opt.Global home name) ->
      defineAlias (CN.global home name) state

    Opt.VarEnum (Opt.Global home name) _ ->
      defineAlias (CN.global home name) state

    Opt.VarBox (Opt.Global home name) ->
      defineAlias (CN.global home name) state

    Opt.VarCycle home name ->
      defineAlias (CN.global home name) state

    Opt.VarDebug name home _ _ ->
      defineAlias (CN.global home name) state

    Opt.VarKernel home name ->
      defineAlias (CN.kernelValue home name) $
      if CK.shouldGenJsEnumId home name then
        addLiteral CL.insertKernelJs (home, name) state
      else
        state

    Opt.VarLocal _ -> error "COMPILER BUG: Global variable cannot also be local"
    Opt.TailCall _ _ -> error "COMPILER BUG: Tail recursive global should be in a DefineTailFunc node rather than a Define node"


generateRuntimeInit :: CN.KernelTypeDef -> Opt.Global -> Opt.Expr -> State -> State
generateRuntimeInit structName global@(Opt.Global home' name') expr state =
  let
    initPtrName =
      CN.globalInitPtr home' name'

    declarePtr :: C.ExternalDeclaration
    declarePtr =
      C.DeclExt $ C.Decl
        [C.TypeSpec $ C.TypeDef structName]
        (Just $ C.Declr (Just initPtrName) [C.PtrDeclr []])
        Nothing

    defineGlobal :: C.ExternalDeclaration
    defineGlobal =
      C.DefineExt (CN.global home' name') $
      C.Parens $ C.Unary C.DerefOp $ C.Var initPtrName
  in
  generateInitFunction global expr $
  addExtDecl declarePtr $
  addExtDecl defineGlobal $
    state


generateInitFunction :: Opt.Global -> Opt.Expr -> State -> State
generateInitFunction global@(Opt.Global home name) body state =
  let
    fname = CN.globalInitFn home name
    exprGenerator = CE.generateInitFunction fname body
    evalFnState = generateExpression global fname exprGenerator state
  in
  evalFnState
    { _revInitGlobals = global : _revInitGlobals evalFnState }


generateGlobalFunction :: Opt.Global -> [Name.Name] -> Opt.Expr -> State -> State
generateGlobalFunction global@(Opt.Global home name) args body state =
  let
    fname =
      CN.globalEvaluator home name

    closure =
      CK.generateClosure
        (CN.global home name)
        (C.Unary C.AddrOp $ C.Var fname)
        (length args)
        []

    exprGenerator = CE.generateEvalFunction fname args body False
    evalFnState = generateExpression global fname exprGenerator state
  in
  addExtDecl closure evalFnState


generateCtor :: Opt.Global -> Int -> State -> State
generateCtor global@(Opt.Global home name) arity state =
  if arity /= 0 then
    generateCtorFn global arity state
  else
    let
      extDecl = CK.generateCustomStruct (CN.global home name) (CN.ctorId name)
    in
    state
      { _revExtDecls = extDecl : (_revExtDecls state)
      , _literals = CL.insertCtor name (_literals state)
      }  


generateCtorFn :: Opt.Global -> Int -> State -> State
generateCtorFn (Opt.Global home name) arity state =
  let
    fname =
      CN.globalEvaluator home name

    ctorCall :: C.Expression
    ctorCall =
      C.Call
        (C.Var $ CN.fromBuilder "newCustom")
        [ C.Var $ CN.ctorId name
        , C.Const $ C.IntConst arity
        , C.Var $ CN.fromBuilder "args"
        ]

    evalFn :: C.ExternalDeclaration
    evalFn =
      C.FDefExt $ C.FunDef
        [C.TypeSpec C.Void]
        (C.Declr (Just fname) [C.PtrDeclr [], C.FunDeclr [C.argsArray]])
        [C.BlockStmt $ C.Return $ Just ctorCall]

    closure :: C.ExternalDeclaration
    closure =
      CK.generateClosure (CN.global home name)
        (C.addrOf fname) arity []
  in
  state
    { _revExtDecls = closure : evalFn : (_revExtDecls state)
    , _literals = CL.insertCtor name (_literals state)
    }


{-----------------------------------------------------------

      HASKELL DEBUG

-----------------------------------------------------------}

traceBuilder :: B.Builder -> a -> a
traceBuilder builder thing =
  Debug.trace
    (show $ B.toLazyByteString builder)
    thing


traceDeps :: B.Builder -> Opt.Node -> Opt.Global -> Set.Set Opt.Global -> Set.Set Opt.Global
traceDeps debugIndent node (Opt.Global home name) deps =
  let
    depBuilders :: [B.Builder]
    depBuilders =
      map
        (\(Opt.Global depHome depName) ->
          CN.toBuilder $ CN.global depHome depName)
        (Set.toList deps)

    message :: B.Builder
    message =
      debugIndent
      <> (nodeName node)
      <> " "
      <> (CN.toBuilder $ CN.global home name)
      <> " ("
      <> (mconcat $ List.intersperse ", " depBuilders)
      <> ")"
  in
  traceBuilder message deps


nodeName :: Opt.Node -> B.Builder
nodeName node =
  case node of
    Opt.Define expr _ -> "Define " <> (exprName expr)
    Opt.DefineTailFunc _ expr _ -> "DefineTailFunc " <> (exprName expr)
    Opt.Ctor _ _ -> "Ctor"
    Opt.Enum _ -> "Enum"
    Opt.Box -> "Box"
    Opt.Link _ -> "Link"
    Opt.Cycle _ _ _ _ -> "Cycle"
    Opt.Manager _ -> "Manager"
    Opt.Kernel _ _ -> "Kernel"
    Opt.PortIncoming _ _ -> "PortIncoming"
    Opt.PortOutgoing _ _ -> "PortOutgoing"


exprName :: Opt.Expr -> B.Builder
exprName expr =
  case expr of    
    Opt.Bool _ -> "Bool"
    Opt.Chr _ -> "Chr"
    Opt.Str _ -> "Str"
    Opt.Int _ -> "Int"
    Opt.Float _ -> "Float"
    Opt.VarLocal _ -> "VarLocal"
    Opt.VarGlobal _ -> "VarGlobal"
    Opt.VarEnum _ _ -> "VarEnum"
    Opt.VarBox _ -> "VarBox"
    Opt.VarCycle _ _ -> "VarCycle"
    Opt.VarDebug _ _ _ _ -> "VarDebug"
    Opt.VarKernel _ _ -> "VarKernel"
    Opt.List _ -> "List"
    Opt.Function _ _ -> "Function"
    Opt.Call _ _ -> "Call"
    Opt.TailCall _ _ -> "TailCall"
    Opt.If _ _ -> "If"
    Opt.Let _ _ -> "Let"
    Opt.Destruct _ _ -> "Destruct"
    Opt.Case _ _ _ _ -> "Case"
    Opt.Accessor _ -> "Accessor"
    Opt.Access _ _ -> "Access"
    Opt.Update _ _ -> "Update"
    Opt.Record _ -> "Record"
    Opt.Unit -> "Unit"
    Opt.Tuple _ _ _ -> "Tuple"
    Opt.Shader _ _ _ -> "Shader"


{-----------------------------------------------------------

      C DEBUG

-----------------------------------------------------------}


generateFunctionDebugNames :: [C.ExternalDeclaration] -> C.ExternalDeclaration
generateFunctionDebugNames allExtDecls =
  let
    evalNames :: [CN.Name]
    evalNames =
      foldr nextEvalFuncName [] allExtDecls

    paramName :: CN.Name
    paramName =
      CN.fromBuilder "p"

    paramDecl :: C.Declaration
    paramDecl =
      C.Decl
        [C.TypeSpec C.Void]
        (Just $ C.Declr (Just paramName) [C.PtrDeclr []])
        Nothing

    ifStmt :: CN.Name -> C.CompoundBlockItem
    ifStmt name =
      C.BlockStmt $ C.If
        (C.Binary C.EqOp (C.Var paramName) (C.Unary C.AddrOp $ C.Var name))
        (C.Compound [C.BlockStmt $ C.Return $ Just $ C.Const $ C.StrConst $ CN.toBuilder name])
        Nothing

    ifs = map ifStmt evalNames
    catchall = C.BlockStmt $ C.Return $ Just $ C.Const $ C.StrConst "(?)"
  in
  C.FDefExt
    (C.FunDef
      [C.TypeSpec C.Char]
      (C.Declr
        (Just $ CN.fromBuilder "Debug_evaluator_name")
        [C.PtrDeclr [], C.FunDeclr [paramDecl]])
      (catchall : ifs))


nextEvalFuncName :: C.ExternalDeclaration -> [CN.Name] -> [CN.Name]
nextEvalFuncName extDecl evalNames =
  case extDecl of
    C.FDefExt
      (C.FunDef
        [C.TypeSpec C.Void]
        (C.Declr (Just fname) [C.PtrDeclr [], C.FunDeclr (_ : [])])
        _ ) ->
          fname : evalNames
    _ ->
      evalNames
