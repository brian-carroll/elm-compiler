{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where

import Prelude hiding (cycle, print)
import qualified Control.Monad.State as State

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Bits as Bits
import qualified Data.Char as Char
import qualified Data.Utf8 as Utf8

import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CN
import qualified Generate.C.Expression as CE
import qualified Generate.C.AST as C
import qualified Generate.C.JsWrappers as JsWrappers
import qualified Generate.C.Kernel as CKernel

import qualified Generate.JavaScript as JS
import qualified Generate.JavaScript.Builder as JSB
import qualified Generate.JavaScript.Name as JSN
import qualified Generate.JavaScript.Functions as JsFunctions

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES

import qualified Generate.Mode as Mode

import Debug.Trace as Debug


-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main


-- GRAPH TRAVERSAL STATE

data State =
  State
    { _seenGlobals :: Set.Set Opt.Global
    , _sharedDefs :: Set.Set CE.SharedDef
    , _revInitGlobals :: [Opt.Global]
    , _revExtDecls :: [C.ExternalDeclaration]
    , _jsState :: JS.State
    }


generate :: Opt.GlobalGraph -> Mains -> (B.Builder, B.Builder)
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    state = Map.foldrWithKey (addMain graph) emptyState mains
    appTypes = extractJsWrapperConfig state
    cBuilder = buildC mains state
    jsBuilder = buildJs appTypes (_jsState state) mains
  in
    (cBuilder, jsBuilder)


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _sharedDefs = Set.singleton $ CE.SharedJsKernel "Json" "run"
    , _revInitGlobals = []
    , _revExtDecls = []
    , _jsState = JS.emptyState
    }


{-----------------------------------------------------------

    JAVASCRIPT FILE

-----------------------------------------------------------}

data JsWrapperConfig =
  JsWrapperConfig
    { appFields :: Set.Set Name.Name
    , appFieldGroups :: [[Name.Name]]
    , appCtors :: [Name.Name]
    , appKernelVals :: [JSN.Name]
    }


buildJs :: JsWrapperConfig -> JS.State -> Mains -> B.Builder
buildJs appEnums jsState mains =
  "var " <> (JSN.toBuilder wasmWrapperName) <> ";\n"
  <> "(function(scope){\n'use strict';"
  <> JsWrappers.defineOnReady
  <> JsWrappers.emscriptenPostRun (
      JsFunctions.functions
      <> JS.stateToBuilder jsState
      <> jsInitWrapper appEnums
      <> jsAssignMains mains
      <> JS.toMainExports jsMode mains
      <> JsWrappers.executeOnReadyCallback
    )
  <> "}(this));"


jsMode :: Mode.Mode
jsMode =
  Mode.Dev Nothing


wasmWrapperName :: JSN.Name
wasmWrapperName =
  JSN.fromLocal $ Name.fromChars "wasmWrapper"


jsAssignMains :: Mains -> B.Builder
jsAssignMains mains =
  let
    (_, builder) =
      Map.foldrWithKey jsAssignMainsHelp (0, "") mains
  in
  builder


jsAssignMainsHelp :: ModuleName.Canonical -> Opt.Main -> (Int, B.Builder) -> (Int, B.Builder)
jsAssignMainsHelp moduleName _ (index, builder) =
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


extractJsWrapperConfig :: State -> JsWrapperConfig
extractJsWrapperConfig state =
  foldr
    extractJsWrapperConfigHelp
    (JsWrapperConfig Set.empty [] [] [])
    (_sharedDefs state)


extractJsWrapperConfigHelp :: CE.SharedDef -> JsWrapperConfig -> JsWrapperConfig
extractJsWrapperConfigHelp def appEnums =
  case def of
    CE.SharedJsKernel home name ->
      appEnums
        { appKernelVals = (JSN.fromKernel home name) : (appKernelVals appEnums)
        }

    CE.SharedJsGlobal (Opt.Global home name) ->
      appEnums
        { appKernelVals = (JSN.fromGlobal home name) : (appKernelVals appEnums)
        }

    CE.SharedFieldGroup fields ->
      appEnums
        { appFields = List.foldr Set.insert (appFields appEnums) fields
        , appFieldGroups = fields : (appFieldGroups appEnums)
        }

    CE.SharedField field ->
      appEnums
        { appFields = Set.insert field (appFields appEnums)
        }

    CE.SharedCtor name ->
      appEnums
        { appCtors = name : (appCtors appEnums)
        }

    _ ->
      appEnums


jsInitWrapper :: JsWrapperConfig -> B.Builder
jsInitWrapper (JsWrapperConfig appFields appFieldGroups appCtors appKernelVals) =
  let
    name =
      JSN.fromLocal . Name.fromChars

    wrapperImportObj =
      JSB.Object $ map
        (\n -> (n, JSB.Ref n))
        JsWrappers.importsFromElm

    fgStrings = map
      (\fNames ->
        JSB.String $
          mconcat $ List.intersperse " " $
          map Name.toBuilder fNames)
      appFieldGroups

    appTypes =
      JSB.Object
        [ ( name "ctors"
          , JSB.Array $ map (JSB.String . Name.toBuilder) appCtors
          )
        , ( name "fields"
          , JSB.Array $ map (JSB.String . Name.toBuilder) (Set.toList appFields)
          )
        , ( name "fieldGroups"
          , JSB.Array fgStrings
          )
        ]

    emscriptenModule =
      JSB.Ref $ name JsWrappers.emscriptenModuleRef
    
    kernelRecord =
      JSB.Object $ map
        (\jsName -> (jsName, JSB.Ref jsName))
        appKernelVals
  in
  JSB.stmtToBuilder $ JSB.ExprStmt $
    JSB.Assign (JSB.LRef wasmWrapperName) $
    JSB.Call (JSB.Ref $ name JsWrappers.wrapWasmElmApp) 
      [ wrapperImportObj
      , JSB.Access (JSB.Access emscriptenModule (name "HEAPU32")) (name "buffer")
      , JSB.Access emscriptenModule (name "asm")
      , appTypes
      , kernelRecord
      ] 


{-----------------------------------------------------------

    C FILE

-----------------------------------------------------------}

buildC :: Mains -> State -> B.Builder
buildC mains state =
  prependExtDecls [C.IncludeExt CN.KernelH] $
  prependSharedDefs (_sharedDefs state) $
  prependExtDecls (_revExtDecls state) $
  prependExtDecls [generateFunctionDebugNames (_revExtDecls state)] $
  prependExtDecls [generateMainsArray mains, C.BlankLineExt] $
  prependExtDecls [jsonRunIndexAssignment] $
  prependExtDecls [generateCMain (_revInitGlobals state), C.BlankLineExt]
    ""


prependExtDecls :: [C.ExternalDeclaration] -> B.Builder -> B.Builder
prependExtDecls revExtDecls monolith =
  List.foldl' (\m ext -> (CB.fromExtDecl ext) <> m) monolith revExtDecls


{-----------------------------------------------------------

    SHARED DEFINITIONS

-----------------------------------------------------------}

prependSharedDefs :: Set.Set CE.SharedDef -> B.Builder -> B.Builder
prependSharedDefs defs builder =
  let
    (jsKernelNames, cCtorNames, elmFields, fieldGroups, decls) =
      Set.foldr' iterateSharedDefs ([], [], Set.empty, [], []) defs
    cFields =
      map CN.fieldId $ Set.toList elmFields
  in
  prependExtDecls (generateEnum jsKernelNames ++ generateEnumDebug "jsValues" jsKernelNames) $
  prependExtDecls (generateEnum cCtorNames ++ generateEnumDebug "ctors" cCtorNames) $
  prependExtDecls (generateEnum cFields ++ generateEnumDebug "fields" cFields) $
  prependExtDecls decls $
  prependExtDecls [generateFieldGroupArray fieldGroups] $
  builder


iterateSharedDefs :: CE.SharedDef
  -> ([CN.Name], [CN.Name], Set.Set Name.Name, [[Name.Name]], [C.ExternalDeclaration])
  -> ([CN.Name], [CN.Name], Set.Set Name.Name, [[Name.Name]], [C.ExternalDeclaration])
iterateSharedDefs def acc@(jsKernelNames, ctorNames, fieldNames, fieldGroups, decls) =
  let newDecls = (generateSharedDefItem def) ++ decls
  in
  case def of
    CE.SharedJsKernel home name ->
      ( (CN.jsKernelEval home name) : jsKernelNames
      , ctorNames
      , fieldNames
      , fieldGroups
      , newDecls
      )
    CE.SharedJsGlobal (Opt.Global home name) ->
      ( (CN.jsGlobalEval home name) : jsKernelNames
      , ctorNames
      , fieldNames
      , fieldGroups
      , newDecls
      )
    CE.SharedFieldGroup fields ->
      ( jsKernelNames
      , ctorNames
      , List.foldr Set.insert fieldNames fields
      , fields : fieldGroups
      , newDecls
      )
    CE.SharedField field ->
      ( jsKernelNames
      , ctorNames
      , Set.insert field fieldNames
      , fieldGroups
      , newDecls
      )
    CE.SharedCtor name ->
      ( jsKernelNames
      , (CN.ctorId name) : ctorNames
      , fieldNames
      , fieldGroups
      , newDecls
      )
    _ ->
      ( jsKernelNames, ctorNames, fieldNames, fieldGroups, newDecls )


generateEnum :: [CN.Name] -> [C.ExternalDeclaration]
generateEnum names =
  case names of
    [] -> []
    _ -> [C.DeclExt $ C.Decl [C.TypeSpec $ C.Enum names] Nothing Nothing]


generateFieldGroupArray :: [[Name.Name]] -> C.ExternalDeclaration
generateFieldGroupArray fieldGroups =
  let
    pointerArray = foldr
      (\fields acc ->
        ([], C.InitExpr $ C.Unary C.AddrOp $ C.Var $ CN.fieldGroup fields)
        : acc
      )
      [([], C.InitExpr $ C.Var CN.nullPtr)]
      fieldGroups
  in
  C.DeclExt $ C.Decl
  [C.TypeSpec $ C.TypeDef CN.FieldGroup]
  (Just $ C.Declr (Just CN.wrapperFieldGroups) [C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
  (Just $ C.InitExpr $ C.CompoundLit $ pointerArray)


jsonRunIndexAssignment :: C.ExternalDeclaration
jsonRunIndexAssignment =
  C.DeclExt $ C.Decl
    [C.TypeSpec C.SizeT]
    (Just $ C.Declr (Just CN.jsonRunEvalIndex) [])
    (Just $ C.InitExpr $ C.Var $ CN.jsKernelEval "Json" "run")


generateSharedDefItem :: CE.SharedDef -> [C.ExternalDeclaration]
generateSharedDefItem def =
  case def of
    CE.SharedInt value ->
      [generateStructDef CN.ElmInt (CN.literalInt value)
        [ ("header", CE.generateHeader CE.HEADER_INT)
        , ("value", C.Const $ C.IntConst value)
        ]
        Nothing
      ]

    CE.SharedFloat value ->
      [generateStructDef CN.ElmFloat (CN.literalFloat value)
        [ ("header", CE.generateHeader CE.HEADER_FLOAT)
        , ("value", C.Const $ C.FloatConst value)
        ]
        Nothing
      ]

    CE.SharedChr value ->
      [generateStructDef CN.ElmChar (CN.literalChr value)
        [("header", CE.generateHeader CE.HEADER_CHAR)]
        (Just ("words16", generateUtf16 value))
      ]

    CE.SharedStr value ->
      let words16 = generateUtf16 value
      in
      [generateStructDef CN.ElmString16 (CN.literalStr value)
        [("header", CE.generateHeader $ CE.HEADER_STRING (length words16))]
        (Just ("words16", words16))
      ]
  
    CE.SharedAccessor name ->
      [generateClosure (CN.accessor name)
        (C.Unary C.AddrOp $ C.Var CN.utilsAccessEval)
        2 [C.nameAsVoidPtr $ CN.fieldId name]
      ]

    CE.SharedFieldGroup names ->
      [generateStructDef CN.FieldGroup (CN.fieldGroup names)
        [ ("header", CE.generateHeader $ CE.HEADER_FIELDGROUP (length names))
        , ("size", C.Const $ C.IntConst $ length names)
        ]
        (Just ("fields", map (C.Var . CN.fieldId) names))
      ]

    CE.SharedField _ ->
      []

    CE.SharedCtor _ ->
      []

    CE.SharedJsKernel home name ->
      if CKernel.shouldGenStruct home name then
        [generateClosure (CN.kernelValue home name)
          (C.nameAsVoidPtr $ CN.jsKernelEval home name)
          maxClosureArity
          []
        ]
      else
        []

    CE.SharedJsGlobal (Opt.Global home name) ->
      [generateClosure (CN.global home name)
        (C.nameAsVoidPtr $ CN.jsGlobalEval home name)
        maxClosureArity
        []
      ]


generateUtf16 :: ES.String -> [C.Expression]
generateUtf16 str =
  map (C.Const . C.IntHexConst) $ concatMap encodeUtf16 (ES.toChars str)


encodeUtf16 :: Char -> [Int]
encodeUtf16 chr =
  let
    codepoint = Char.ord chr
    (high, low) = quotRem (codepoint - 0x10000) 0x400
  in
  if codepoint < 0x10000 then
    [codepoint]
  else
    [ high + 0xD800
    , low + 0xDC00
    ]


maxClosureArity :: Int
maxClosureArity =
  0xffff


generateClosure :: CN.Name -> C.Expression -> Int -> [C.Expression] -> C.ExternalDeclaration
generateClosure name evalFnPtr maxValues values =
  let nValues = length values
  in
  generateStructDef CN.Closure name
    [ ("header", CE.generateHeader $ CE.HEADER_CLOSURE nValues)
    , ("n_values", C.Const $ C.IntHexConst nValues)
    , ("max_values", C.Const $ C.IntHexConst maxValues)
    , ("evaluator", evalFnPtr)
    ]
    (if nValues > 0 then Just ("values", values) else Nothing)


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


{-----------------------------------------------------------

    C PROGRAM INITIALISATION

-----------------------------------------------------------}

generateCMain :: [Opt.Global] -> C.ExternalDeclaration
generateCMain revInitGlobals =
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
    initKernelEvalIds =
      map initKernelEvalId
    initCalls =
      List.foldl' generateInitCall [] revInitGlobals
    runGC =
      C.BlockStmt $ C.Expr $ Just $
      C.Call (C.Var $ CN.fromBuilder "GC_collect_full")
      []
    returnSuccess =
      C.BlockStmt $ C.Return $ Just $ C.Const (C.IntConst 0)
    body =
      [ initGC
      , returnFail
      ] ++
      initCalls ++
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


initKernelEvalId :: CE.SharedDef -> [C.CompoundBlockItem]
initKernelEvalId def =
  case def of
    CE.SharedJsKernel home name ->
      [C.BlockStmt $ C.Expr $ Just $ C.Assign C.AssignOp
        (C.Index
          (C.MemberDot
            (C.Var $ CN.kernelValue home name)
            (CN.fromBuilder "values"))
          (C.Const $ C.IntConst 0)
        )
        (C.nameAsVoidPtr $ CN.jsKernelEval home name)
      ]

    _ ->
      []


generateInitCall :: [C.CompoundBlockItem] -> Opt.Global -> [C.CompoundBlockItem]
generateInitCall acc (Opt.Global home name) =
  let
    initCall = C.BlockStmt $ C.Expr $ Just $
      C.Call (C.Var CN.utilsInitGlobal)
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
        (JS.addGlobal "" jsMode graph)
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
      generateManager global effectsType $
      state { _jsState =
        JS.addGlobal debugIndent jsMode graph (_jsState state) global
      }

    Opt.Kernel chunks deps ->
      let
        (Opt.Global home@(ModuleName.Canonical _ moduleName) _) = global
        depState =
          if moduleName == Name.debugger then
            state -- haven't written debugger dependencies in C yet!
          else
            addDeps deps state
      in
      if CKernel.shouldGenJsCode home then
        depState { _jsState =
          JS.addGlobal debugIndent jsMode graph (_jsState state) global
        }
      else
        depState

    Opt.Enum _ ->
      generateCtor global 0 state

    Opt.Box ->
      generateCtor global 1 state

    Opt.PortIncoming decoder deps ->
      addShared (CE.SharedJsGlobal global) $
      addDeps deps $
      state { _jsState =
        JS.addGlobal debugIndent jsMode graph (_jsState state) global
      }

    Opt.PortOutgoing encoder deps ->
      addShared (CE.SharedJsGlobal global) $
      addDeps deps $
      state { _jsState =
        JS.addGlobal debugIndent jsMode graph (_jsState state) global
      }


addExtDecl :: C.ExternalDeclaration -> State -> State
addExtDecl extDecl state =
  state { _revExtDecls = extDecl : _revExtDecls state }


addShared :: CE.SharedDef -> State -> State
addShared sharedDef state =
  state { _sharedDefs =
    Set.insert sharedDef (_sharedDefs state) }

    
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
    tailFnName = CN.globalTailEvaluator home name
    wrapFnName = CN.globalEvaluator home name
    closureName = CN.global home name

    initExprState =
      CE.initState global (_revExtDecls state) (_sharedDefs state)

    (revExtDecls, sharedDefs) =
      CE.globalDefsFromExprState $
      State.execState
        (CE.generateTailDefEval tailFnName wrapFnName args body)
        initExprState

    closure =
      generateClosure closureName (C.addrOf wrapFnName) (length args) []
  in
  state
    { _revExtDecls = closure : revExtDecls
    , _sharedDefs = sharedDefs
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

    initExprState =
      CE.initState
        global
        (declarePtr : defineAlias : _revExtDecls state)
        (_sharedDefs state)

    (revExtDecls, sharedDefs) =
      CE.globalDefsFromExprState $
      State.execState
        (CE.generateCycleFn ptrName funcName expr)
        initExprState
  in
  state
    { _revExtDecls = revExtDecls
    , _sharedDefs = sharedDefs
    , _revInitGlobals = global : (_revInitGlobals state)
    }


{-----------------------------------------------------------

                EFFECT MANAGER

-----------------------------------------------------------}

generateManager :: Opt.Global -> Opt.EffectsType -> State -> State
generateManager global@(Opt.Global home _) effectsType state =
  let
    (ModuleName.Canonical _ moduleName) = home

    (Utf8.Utf8 moduleNameBytes) = moduleName
    moduleNameStr = Utf8.Utf8 moduleNameBytes -- different phantom type

    makeClosure name =
      generateClosure (CN.global home name)
        (C.nameAsVoidPtr $ CN.jsKernelEval Name.platform "leaf")
        maxClosureArity
        [C.addrOf $ CN.literalStr moduleNameStr]

    closures =
      map makeClosure $
      case effectsType of
        Opt.Cmd -> ["command"]
        Opt.Sub -> ["subscription"]
        Opt.Fx -> ["subscription", "command"]  
  in
  addShared (CE.SharedStr moduleNameStr) $
  addShared (CE.SharedJsKernel Name.platform "leaf") $
    state
      { _revExtDecls =
          closures ++ (_revExtDecls state)
      }


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
      let
        fname = CN.globalEvaluator home' name'
        closure = generateClosure
          globalName
          (C.Unary C.AddrOp $ C.Var fname)
          (length args)
          []
      in
      addExtDecl closure $
      generatExtFunc global fname (Just args) body state

    Opt.Int value ->
      addShared (CE.SharedInt value) $
        defineAlias (CN.literalInt value) state

    Opt.Float value ->
      addShared (CE.SharedFloat value) $
        defineAlias (CN.literalFloat value) state
  
    Opt.Chr value ->
      addShared (CE.SharedChr value) $
        defineAlias (CN.literalChr value) state

    Opt.Str value ->
      addShared (CE.SharedStr value) $
        defineAlias (CN.literalStr value) state

    Opt.Bool bool ->
      defineAlias (if bool then CN.true else CN.false) state

    Opt.Unit ->
      defineAlias CN.unit state

    Opt.Accessor name ->
      addShared (CE.SharedField name) $
      addShared (CE.SharedAccessor name) $
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
      if CKernel.shouldGenJsEnumId home name then
        addShared (CE.SharedJsKernel home name) state
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
  generateInitFn global expr $
  addExtDecl declarePtr $
  addExtDecl defineGlobal $
    state


generatExtFunc :: Opt.Global -> CN.Name -> Maybe [Name.Name] -> Opt.Expr -> State -> State
generatExtFunc global fname params body state =
  let
    initExprState =
      CE.initState global (_revExtDecls state) (_sharedDefs state)

    (revExtDecls, sharedDefs) =
      CE.globalDefsFromExprState $
      State.execState
        (CE.generateEvalFn fname params body False)
        initExprState
  in
  state
    { _revExtDecls = revExtDecls
    , _sharedDefs = sharedDefs
    }


generateInitFn :: Opt.Global -> Opt.Expr -> State -> State
generateInitFn global@(Opt.Global home name) body state =
  let
    fname = CN.globalInitFn home name
  in
  generatExtFunc global fname Nothing body $
    state { _revInitGlobals = global : _revInitGlobals state }


generateCtor :: Opt.Global -> Int -> State -> State
generateCtor global@(Opt.Global home name) arity state =
  if arity /= 0 then
    generateCtorFn global arity state
  else
    let
      extDecl =
        generateStructDef
          CN.Custom
          (CN.global home name)
          [ ("header", CE.generateHeader $ CE.HEADER_CUSTOM 0)
          , ("ctor", C.Var $ CN.ctorId name)
          ]
          Nothing
    in
    state
      { _revExtDecls = extDecl : (_revExtDecls state)
      , _sharedDefs = Set.insert (CE.SharedCtor name) (_sharedDefs state)
      }  


generateCtorFn :: Opt.Global -> Int -> State -> State
generateCtorFn (Opt.Global home name) arity state =
  let
    fname =
      CN.globalEvaluator home name

    ctorCustomCall :: C.Expression
    ctorCustomCall =
      C.Call
        (C.Var $ CN.fromBuilder "ctorCustom")
        [ C.Var $ CN.ctorId name
        , C.Const $ C.IntConst arity
        , C.Var $ CN.fromBuilder "args"
        ]

    evalFn :: C.ExternalDeclaration
    evalFn =
      C.FDefExt $ C.FunDef
        [C.TypeSpec C.Void]
        (C.Declr (Just fname) [C.PtrDeclr [], C.FunDeclr [C.argsArray]])
        [C.BlockStmt $ C.Return $ Just ctorCustomCall]

    closure :: C.ExternalDeclaration
    closure =
      generateClosure (CN.global home name)
        (C.addrOf fname) arity []
  in
  state
    { _revExtDecls = closure : evalFn : (_revExtDecls state)
    , _sharedDefs = Set.insert (CE.SharedCtor name) (_sharedDefs state)
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


generateEnumDebug :: B.Builder -> [CN.Name] -> [C.ExternalDeclaration]
generateEnumDebug arraySuffix names =
  let
    arrayName :: B.Builder
    arrayName = "Debug_" <> arraySuffix

    strings :: C.InitializerList
    strings =
      map
        (\name ->
          ([], C.InitExpr $ C.Const $ C.StrConst (CN.toBuilder name)))
        names
    
    array :: C.ExternalDeclaration
    array =
      C.DeclExt $ C.Decl
        [C.TypeSpec C.Char]
        (Just $ C.Declr (Just $ CN.fromBuilder arrayName)
          [C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
        (Just $ C.InitExpr $ C.CompoundLit strings)

    size :: C.ExternalDeclaration
    size =
      C.DeclExt $ C.Decl
        [C.TypeSpec C.Int]
        (Just $ C.Declr (Just $ CN.fromBuilder (arrayName <> "_size")) [])
        (Just $ C.InitExpr $ C.Const $ C.IntConst $ length names)
  in
  [ size
  , array
  ]


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

    ifClause :: CN.Name -> C.Statement -> C.Statement
    ifClause name elseStmt =
      C.If
        (C.Binary C.EqOp (C.Var paramName) (C.Unary C.AddrOp $ C.Var name))
        (C.Compound [C.BlockStmt $
          C.Return $ Just $ C.Const $ C.StrConst $ CN.toBuilder name])
        (Just elseStmt)

    bigIf :: C.Statement
    bigIf =
      foldr ifClause 
        (C.Return $ Just $ C.Const $ C.StrConst "(?)")
        evalNames
  in
  C.FDefExt
    (C.FunDef
      [C.TypeSpec C.Char]
      (C.Declr
        (Just $ CN.fromBuilder "Debug_evaluator_name")
        [C.PtrDeclr [], C.FunDeclr [paramDecl]])
      [C.BlockStmt bigIf])


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
