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

import qualified Generate.JavaScript as JS
import qualified Generate.JavaScript.Builder as JSB
import qualified Generate.JavaScript.Name as JSN
import qualified Generate.JavaScript.Functions as JsFunctions

-- import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
-- import qualified Data.Index as Index
-- import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES

import qualified Generate.Mode as Mode
-- import qualified Reporting.Doc as D
-- import qualified Reporting.Render.Type as RT
-- import qualified Reporting.Render.Type.Localizer as L

import Debug.Trace as Debug


traceBuilder :: B.Builder -> a -> a
traceBuilder builder thing =
  Debug.trace
    (show $ B.toLazyByteString builder)
    thing


traceGlobalNode :: Opt.Global -> Opt.Node -> Opt.Node
traceGlobalNode (Opt.Global home name) node =
  let
    tab = "    "
    msg =
      (nodeName node)
      <> tab
      <> (CN.toBuilder $ CN.global home name)
  in
  traceBuilder msg node


traceDeps :: Set.Set Opt.Global -> Set.Set Opt.Global
traceDeps deps =
  let
    depBuilders :: [B.Builder]
    depBuilders =
      map
        (\(Opt.Global depHome depName) ->
          CN.toBuilder $ CN.global depHome depName)
        (Set.toList deps)

    message :: B.Builder
    message =
      "deps: " <>
      (mconcat $ List.intersperse "," depBuilders)
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


-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main


-- GRAPH TRAVERSAL STATE

data State =
  State
    { _seenGlobals :: Set.Set Opt.Global
    , _sharedDefs :: Set.Set CE.SharedDef
    , _ctors :: Set.Set Name.Name
    , _revInitGlobals :: [Opt.Global]
    , _revExtDecls :: [C.ExternalDeclaration]
    , _jsState :: JS.State
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _sharedDefs = Set.empty
    , _ctors = Set.empty
    , _revInitGlobals = []
    , _revExtDecls = []
    , _jsState = JS.emptyState
    }


jsMode :: Mode.Mode
jsMode =
  Mode.Dev Nothing


data AppEnums =
  AppEnums
    { appFields :: Set.Set Name.Name
    , appFieldGroups :: [[Name.Name]]
    , appCtors :: [Name.Name]
    , appKernelVals :: [(Name.Name, Name.Name)]
    }
    

generate :: Opt.GlobalGraph -> Mains -> (B.Builder, B.Builder)
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    state = Map.foldrWithKey (addMain graph) emptyState mains
    appTypes = extractAppEnums state
    cBuilder = buildC mains state
    jsBuilder = buildJs appTypes (_jsState state) mains
  in
    (cBuilder, jsBuilder)


extractAppEnums :: State -> AppEnums
extractAppEnums state =
  foldr
    extractAppEnumsHelp
    (AppEnums Set.empty [] (Set.toList $ _ctors state) [])
    (_sharedDefs state)


extractAppEnumsHelp :: CE.SharedDef -> AppEnums -> AppEnums
extractAppEnumsHelp def appEnums =
  case def of
    CE.SharedJsThunk home name ->
      appEnums
        { appKernelVals = (home, name) : (appKernelVals appEnums)
        }
      
    CE.SharedFieldGroup fields ->
      appEnums
        { appFields = List.foldr Set.insert (appFields appEnums) fields
        , appFieldGroups = fields : (appFieldGroups appEnums)
        }

    _ ->
      appEnums


buildC :: Mains -> State -> B.Builder
buildC mains state =
  let
    ctorNames = map CN.ctorId $ Set.toList $ _ctors state
  in
  prependExtDecls [C.IncludeExt CN.KernelH] $
  prependExtDecls (generateEnum ctorNames) $
  prependSharedDefs (_sharedDefs state) $
  prependExtDecls (_revExtDecls state) $
  prependExtDecls [generateMainsArray mains, C.BlankLineExt] $
  prependExtDecls [generateCMain (_revInitGlobals state), C.BlankLineExt]
    ""


prependExtDecls :: [C.ExternalDeclaration] -> B.Builder -> B.Builder
prependExtDecls revExtDecls monolith =
  List.foldl' (\m ext -> (CB.fromExtDecl ext) <> m) monolith revExtDecls


{-
    JS top level
-}

buildJs :: AppEnums -> JS.State -> Mains -> B.Builder
buildJs appEnums jsState mains =
  let
    emscriptenModule =
      B.string8 JsWrappers.emscriptenModuleName
  in
  "(function(scope){\n'use strict';"
  <> JsWrappers.emscripten
  <> (emscriptenModule <> ".postRun = function() {\n")
  <> JsFunctions.functions
  <> JS.stateToBuilder jsState
  <> JsWrappers.wrapper
  <> jsInitWrapper appEnums
  <> jsAssignMains mains
  <> JS.toMainExports jsMode mains
  <> "}\n"
  <> "}(this));"


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


jsInitWrapper :: AppEnums -> B.Builder
jsInitWrapper (AppEnums appFields appFieldGroups appCtors appKernelVals) =
  let
    name =
      JSN.fromLocal . Name.fromChars

    fgStrings = map
      (\fNames ->
        JSB.String $
          mconcat $ List.intersperse "$" $
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
      JSB.Ref $ name JsWrappers.emscriptenModuleName
  in
  JSB.stmtToBuilder $
    JSB.Var wasmWrapperName $
    JSB.Call (JSB.Ref $ name JsWrappers.wrapperFnName) 
      [ JSB.Access emscriptenModule (name "buffer")
      , JSB.Access emscriptenModule (name "asm")
      , appTypes
      , JSB.Array $ map (JSB.Ref . (uncurry JSN.fromKernel)) appKernelVals  
      ] 


{-
    Shared definitions at top of file
-}

prependSharedDefs :: Set.Set CE.SharedDef -> B.Builder -> B.Builder
prependSharedDefs defs builder =
  let
    (jsKernelNames, elmFields, fieldGroups, decls) =
      Set.foldr' iterateSharedDefs ([], Set.empty, [], []) defs
    cFields =
      map CN.fieldId $ Set.toList elmFields
  in
  prependExtDecls (generateEnum jsKernelNames) $
  prependExtDecls (generateEnum cFields) $
  prependExtDecls decls $
  prependExtDecls [generateFieldGroupArray fieldGroups] $
  builder


iterateSharedDefs :: CE.SharedDef
  -> ([CN.Name], Set.Set Name.Name, [[Name.Name]], [C.ExternalDeclaration])
  -> ([CN.Name], Set.Set Name.Name, [[Name.Name]], [C.ExternalDeclaration])
iterateSharedDefs def acc@(jsKernelNames, fieldNames, fieldGroups, decls) =
  let newDecls = (generateSharedDefItem def) : decls
  in
  case def of
    CE.SharedJsThunk home name ->
      ( (CN.jsKernelEval home name) : jsKernelNames
      , fieldNames
      , fieldGroups
      , newDecls
      )
    CE.SharedFieldGroup fields ->
      ( jsKernelNames
      , List.foldr Set.insert fieldNames fields
      , fields : fieldGroups
      , newDecls
      )
    _ ->
      ( jsKernelNames, fieldNames, fieldGroups, newDecls )


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
  (Just $ C.Declr (Just $ CN.appFieldGroups) [C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
  (Just $ C.InitExpr $ C.CompoundLit $ pointerArray)
    

generateSharedDefItem :: CE.SharedDef -> C.ExternalDeclaration
generateSharedDefItem def =
  case def of
    CE.SharedInt value ->
      generateStructDef CN.ElmInt (CN.literalInt value)
        [ ("header", CE.generateHeader CE.HEADER_INT)
        , ("value", C.Const $ C.IntConst value)
        ]
        Nothing

    CE.SharedFloat value ->
      generateStructDef CN.ElmFloat (CN.literalFloat value)
        [ ("header", CE.generateHeader CE.HEADER_FLOAT)
        , ("value", C.Const $ C.FloatConst value)
        ]
        Nothing

    CE.SharedChr value ->
      generateStructDef CN.ElmChar (CN.literalChr value)
        [("header", CE.generateHeader CE.HEADER_CHAR)]
        (Just ("words16", generateUtf16 value))

    CE.SharedStr value ->
      let words16 = generateUtf16 value
      in
      generateStructDef CN.ElmString16 (CN.literalStr value)
        [("header", CE.generateHeader $ CE.HEADER_STRING (length words16))]
        (Just ("words16", words16))
  
    CE.SharedAccessor name ->
      generateClosure (CN.accessor name)
        (C.Unary C.AddrOp $ C.Var CN.utilsAccessEval)
        2 [C.nameAsVoidPtr $ CN.fieldId name]

    CE.SharedFieldGroup names ->
      generateStructDef CN.FieldGroup (CN.fieldGroup names)
        [("size", C.Const $ C.IntConst $ length names)]
        (Just ("fields", map (C.Var . CN.fieldId) names))

    CE.SharedJsThunk home name ->
      generateClosure (CN.kernelValue home name)
        (C.nameAsVoidPtr $ CN.jsKernelEval home name)
        maxClosureArity
        []


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


{-
    C 'main' function (program initialisation)
-}

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
    fwdInitCalls =
      List.foldl' generateInitCall [] revInitGlobals
    registerFieldGroups =
      C.BlockStmt $ C.Expr $ Just $
      C.Call (C.Var CN.wrapperRegisterFieldGroups) [C.Var CN.appFieldGroups]
    registerMains =
      C.BlockStmt $ C.Expr $ Just $
      C.Call (C.Var CN.wrapperRegisterMains) [C.Var CN.mains]
    returnSuccess =
      C.BlockStmt $ C.Return $ Just $ C.Const (C.IntConst 0)
    body =
      [ initGC
      , returnFail
      ] ++
      fwdInitCalls ++
      [ registerFieldGroups
      , registerMains
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
    (Just $ C.Declr (Just CN.mains)
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
      C.Call (C.Var CN.utilsInitGlobal)
      [ C.Unary C.AddrOp $ C.Var $ CN.globalInitPtr home name
      , C.Unary C.AddrOp $ C.Var $ CN.globalInitFn home name
      ]
  in
  initCall : acc




{-
                ELM 'MAIN' VALUES
-}

addMain :: Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain graph home _ state =
  addGlobal graph state (Opt.Global home "main")


addGlobal :: Graph -> State -> Opt.Global -> State
addGlobal graph state global =
  let
    seen = _seenGlobals state
  in
  if Set.member global seen then
    state
  else
    addGlobalHelp graph global $
      state
        { _seenGlobals = Set.insert global seen
        , _revExtDecls = C.BlankLineExt : _revExtDecls state
        }


addGlobalHelp :: Graph -> Opt.Global -> State -> State
addGlobalHelp graph global state =
  let
    addDeps deps someState =
      Set.foldl' (addGlobal graph) someState deps -- (traceDeps deps)
    node =
      -- traceGlobalNode global $
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
      addGlobal graph state linkedGlobal

    Opt.Cycle names values functions deps ->
      generateCycle global names values functions $
      addDeps deps state

    Opt.Manager effectsType ->
      generateManager global effectsType $
      state { _jsState =
        JS.addGlobal jsMode graph (_jsState state) global
      }

    Opt.Kernel chunks deps ->
      let (Opt.Global home _) = global
      in
      if Set.member home cKernelModules then
        state  -- do nothing! handled in C via #include
      else
        state { _jsState =
          JS.addGlobal jsMode graph (_jsState state) global }

    Opt.Enum _ ->
      generateCtor global 0 state

    Opt.Box ->
      generateCtor global 1 state

    Opt.PortIncoming decoder deps ->
      generatePort True global decoder $
      addDeps deps state

    Opt.PortOutgoing encoder deps ->
      generatePort False global encoder $
      addDeps deps state


cKernelModules :: Set.Set ModuleName.Canonical
cKernelModules =
  Set.fromList
    [ ModuleName.basics
    , ModuleName.list
    , ModuleName.string
    , ModuleName.char
    ]


addExtDecl :: C.ExternalDeclaration -> State -> State
addExtDecl extDecl state =
  state { _revExtDecls = extDecl : _revExtDecls state }


addShared :: CE.SharedDef -> State -> State
addShared sharedDef state =
  state { _sharedDefs =
    Set.insert sharedDef (_sharedDefs state) }

    
{-
                CYCLE
-}

generateCycle :: Opt.Global -> [Name.Name] -> [(Name.Name, Opt.Expr)] -> [Opt.Def] -> State -> State
generateCycle (Opt.Global home _) names values functions prevState =
  let
    preDeclsState :: State
    preDeclsState =
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

    functionsState :: State
    functionsState =
      List.foldl'
        (\state def ->
          case def of
            Opt.Def name body ->
              addDef (Opt.Global home name) body state

            Opt.TailDef name args expr ->
              -- TODO
              addExtDecl (C.CommentExt $ Name.toBuilder name) state
        )
        preDeclsState
        functions
  in
  List.foldl' (generateCycleVal home) functionsState values


generateCycleVal :: ModuleName.Canonical -> State -> (Name.Name, Opt.Expr) -> State
generateCycleVal home state (name, expr) =
  let
    global = Opt.Global home name
    globalName = CN.global home name
    ptrName = CN.globalInitPtr home name
    funcName = CN.cycleVar home name

    declarePtr =
      C.DeclExt $ C.Decl
        [C.TypeSpec $ C.TypeDef CN.ElmValue]
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

    
{-
                PORT
-}

generatePort :: Bool -> Opt.Global -> Opt.Expr -> State -> State
generatePort isIncoming global@(Opt.Global home name) expr state =
  let
    kernelFuncName =
      if isIncoming then
        "incomingPort"
      else
        "outgoingPort"

    (Utf8.Utf8 portNameBytes) = name
    portNameString = Opt.Str (Utf8.Utf8 portNameBytes)

    call =
      Opt.Call
        (Opt.VarKernel Name.platform kernelFuncName)
        [portNameString, expr]
  in
  generateRuntimeInit CN.Closure global call state



{-
                EFFECT MANAGER
-}

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
  addShared (CE.SharedJsThunk Name.platform "leaf") $
    state
      { _revExtDecls =
          closures ++ (_revExtDecls state)
      }


{-
                GLOBAL DEFINITION
-}


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
      generatExtFunc global fname args body state

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
      addShared (CE.SharedAccessor name) $
        defineAlias (CN.accessor name) state

    Opt.List _        -> generateRuntimeInit CN.Cons global expr state
    Opt.Call _ _      -> generateRuntimeInit CN.ElmValue global expr state
    Opt.If _ _        -> generateRuntimeInit CN.ElmValue global expr state
    Opt.Let _ _       -> generateRuntimeInit CN.ElmValue global expr state
    Opt.Destruct _ _  -> generateRuntimeInit CN.ElmValue global expr state
    Opt.Case _ _ _ _  -> generateRuntimeInit CN.ElmValue global expr state
    Opt.Access _ _    -> generateRuntimeInit CN.ElmValue global expr state
    Opt.Record _      -> generateRuntimeInit CN.Record global expr state
    Opt.Update _ _    -> generateRuntimeInit CN.Record global expr state
    Opt.Shader _ _ _  -> generateRuntimeInit CN.ElmValue global expr state
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
      if Set.member home' cKernelModules then
        state
      else
        addShared (CE.SharedJsThunk home name) state

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


generatExtFunc :: Opt.Global -> CN.Name -> [Name.Name] -> Opt.Expr -> State -> State
generatExtFunc global fname params body state =
  let
    initExprState =
      CE.initState global (_revExtDecls state) (_sharedDefs state)

    (revExtDecls, sharedDefs) =
      CE.globalDefsFromExprState $
      State.execState
        (CE.generateEvalFn fname params body)
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
  generatExtFunc global fname [] body $
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
      , _ctors = Set.insert name (_ctors state)
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
    , _ctors = Set.insert name (_ctors state)
    }
