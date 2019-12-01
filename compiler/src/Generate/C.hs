{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where

import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Set (Set)
import qualified Data.Set as Set
-- import qualified Data.Utf8 as Utf8

import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CN
import qualified Generate.C.Expression as CE
import qualified Generate.C.AST as C

import qualified Generate.JavaScript as JS

-- import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
-- import qualified Data.Index as Index
-- import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
-- import qualified Generate.JavaScript.Builder as JS
-- import qualified Generate.JavaScript.Expression as Expr
-- import qualified Generate.JavaScript.Functions as Functions
-- import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
-- import qualified Reporting.Doc as D
-- import qualified Reporting.Render.Type as RT
-- import qualified Reporting.Render.Type.Localizer as L



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
    , _fieldGroups :: Set.Set [Name.Name]
    , _ctorNames :: Set.Set Name.Name
    , _jsState :: JS.State
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _sharedDefs = Set.empty
    , _revInitGlobals = []
    , _revExtDecls = []
    , _fieldGroups = Set.empty
    , _ctorNames = Set.empty
    , _jsState = JS.emptyState
    }


generate :: Opt.GlobalGraph -> Mains -> (B.Builder, B.Builder)
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    state = Map.foldrWithKey (addMain graph) emptyState mains
    cBuilder = stateToBuilder state
    jsBuilder = JS.stateToBuilder (_jsState state)
  in
    (cBuilder, jsBuilder)


stateToBuilder :: State -> B.Builder
stateToBuilder state =
  let
    ctorNames =
      map CN.ctorId $ Set.toList $ _ctorNames state

    fieldNames =
      map CN.fieldId $ Set.toList $
      Set.foldl' (List.foldl' $ flip Set.insert) Set.empty $
      _fieldGroups state

    jsKernelNames =
      Set.foldr
        (\def acc ->
          case def of
            CE.SharedJsThunk home name -> (CN.jsKernelEval home name) : acc
            _ -> acc
        )
        [] (_sharedDefs state)

    sharedDefDecls =
      Set.foldl'
        (\acc def -> (generateSharedDef def) : acc)
        [] (_sharedDefs state)
  in
  prependExtDecls [C.IncludeExt CN.KernelH] $
  prependExtDecls (generateEnum ctorNames) $
  prependExtDecls (generateEnum fieldNames) $
  prependExtDecls (generateEnum jsKernelNames) $
  prependExtDecls sharedDefDecls $
  prependExtDecls [generateFieldGroupArray (_fieldGroups state)] $
  prependExtDecls (_revExtDecls state) $
  prependExtDecls [generateCMain (_revInitGlobals state), C.BlankLineExt] $
    ""


prependExtDecls :: [C.ExternalDeclaration] -> B.Builder -> B.Builder
prependExtDecls revExtDecls monolith =
  List.foldl' (\m ext -> (CB.fromExtDecl ext) <> m) monolith revExtDecls


{-
    Accumulated values
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
    returnSuccess =
      C.BlockStmt $ C.Return $ Just $ C.Const (C.IntConst 0)
    body =
      [ initGC
      , returnFail
      ] ++
      fwdInitCalls ++
      [ registerFieldGroups
      , returnSuccess
      ]
  in
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Int]
    (C.Declr (Just $ CN.fromBuilder "main") [C.FunDeclr []]) $
    (List.reverse body)


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


generateEnum :: [CN.Name] -> [C.ExternalDeclaration]
generateEnum names =
  case names of
    [] -> []
    _ -> [C.DeclExt $ C.Decl [C.TypeSpec $ C.Enum names] Nothing Nothing]


generateSharedDef :: CE.SharedDef -> C.ExternalDeclaration
generateSharedDef def =
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
      C.CommentExt "SharedChr"

    CE.SharedStr value ->
      C.CommentExt "SharedStr"

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
        0xffff  -- ridiculously high arity (never evaluate in C)
        []      -- no applied args


generateClosure :: CN.Name -> C.Expression -> Int -> [C.Expression] -> C.ExternalDeclaration
generateClosure name evalFnPtr maxValues values =
  let nValues = length values
  in
  generateStructDef CN.Closure name
    [ ("header", CE.generateHeader $ CE.HEADER_CLOSURE nValues)
    , ("n_values", C.Const $ C.IntConst nValues)
    , ("max_values", C.Const $ C.IntConst maxValues)
    , ("evaluator", evalFnPtr)
    ]
    (if nValues > 0 then Just ("values", values) else Nothing)


generateStructDef :: CN.KernelTypeDef -> CN.Name -> [(B.Builder, C.Expression)] -> Maybe (B.Builder, [C.Expression]) -> C.ExternalDeclaration
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


generateFieldGroupArray :: Set.Set [Name.Name] -> C.ExternalDeclaration
generateFieldGroupArray fieldGroups =
  let
    pointerArray = Set.foldr
      (\fields acc -> ([], C.InitExpr $ C.Unary C.AddrOp $ C.Var $ CN.fieldGroup fields) : acc)
      [([], C.InitExpr $ C.Var CN.nullPtr)]
      fieldGroups
  in
  C.DeclExt $ C.Decl
  [C.TypeSpec $ C.TypeDef CN.FieldGroup]
  (Just $ C.Declr (Just $ CN.appFieldGroups) [C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
  (Just $ C.InitExpr $ C.CompoundLit $ pointerArray)



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
      Set.foldl' (addGlobal graph) someState deps
    jsMode = Mode.Dev Nothing
  in
  case graph ! global of
    Opt.Define expr deps ->
      addDef global expr $
      (addDeps deps state)

    Opt.DefineTailFunc argNames body deps ->
      addDeps deps state

    Opt.Ctor index arity ->
      state

    Opt.Link (Opt.Global moduleName name) ->
      state

    Opt.Cycle names values functions deps ->
      addDeps deps state

    Opt.Manager effectsType ->
      state { _jsState =
        JS.addGlobal jsMode graph (_jsState state) global }

    Opt.Kernel chunks deps ->
      let (Opt.Global home _) = global
      in
      if Set.member home cKernelModules then
        state  -- do nothing! handled in C via #include
      else
        state { _jsState =
          JS.addGlobal jsMode graph (_jsState state) global }

    Opt.Enum index ->
      state

    Opt.Box ->
      state

    Opt.PortIncoming decoder deps ->
      addDeps deps state

    Opt.PortOutgoing encoder deps ->
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
                GLOBAL DEFINITION
-}


addDef :: Opt.Global -> Opt.Expr -> State -> State
addDef global@(Opt.Global home' name') expr state =
  let
    globalName =
      CN.global home' name'

    defineAlias alias state =
      addExtDecl (C.DefineExt globalName $ C.Var alias) state 

    initPtrName =
      CN.globalInitPtr home' name'

    runtimeInit =
      generateInitFn global expr $
      addExtDecl (C.DeclExt $ C.Decl
        [C.TypeSpec $ C.TypeDef CN.ElmValue]
        (Just $ C.Declr (Just initPtrName) [C.PtrDeclr []])
        Nothing
      ) $
      addExtDecl
        (C.DefineExt globalName $ C.Parens $
          C.Unary C.DerefOp $ C.Var initPtrName)
        state
  in
  case expr of
    Opt.Function args body ->
      let
        closure = generateClosure
          globalName
          (C.Unary C.AddrOp $ C.Var $ CN.globalEvaluator home' name')
          (length args)
          []
      in
      addExtDecl closure $
        generateEvalFn global args body state

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

    Opt.List _ -> runtimeInit
    Opt.Call _ _ -> runtimeInit
    Opt.If _ _ -> runtimeInit
    Opt.Let _ _ -> runtimeInit
    Opt.Destruct _ _ -> runtimeInit
    Opt.Case _ _ _ _ -> runtimeInit
    Opt.Access _ _ -> runtimeInit
    Opt.Record _ -> runtimeInit
    Opt.Update _ _ -> runtimeInit
    Opt.Tuple _ _ _ -> runtimeInit
    Opt.Shader _ _ _ -> runtimeInit

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

    -- impossible in global scope
    Opt.VarLocal _ -> undefined
    Opt.TailCall _ _ -> undefined


generateEvalFn :: Opt.Global -> [Name.Name] -> Opt.Expr -> State -> State
generateEvalFn global@(Opt.Global home name) params expr state =
  let
    (bodyState, revBody) = 
      generateFuncBody global params expr state
    
    argsArray :: C.Declaration
    argsArray =
      C.Decl
        [C.TypeSpec C.Void]
        (Just $ C.Declr
          (Just CN.args)
          [C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
        Nothing

    evalFn :: C.ExternalDeclaration
    evalFn = C.FDefExt $ C.FunDef
      [C.TypeSpec C.Void]
      (C.Declr (Just $ CN.globalEvaluator home name)
          [C.PtrDeclr [], C.FunDeclr [argsArray]])
        revBody
  in
  addExtDecl evalFn bodyState


generateInitFn :: Opt.Global -> Opt.Expr -> State -> State
generateInitFn global@(Opt.Global home name) expr state =
  let
    (bodyState, revBody) = 
      generateFuncBody global [] expr state

    initFn :: C.ExternalDeclaration
    initFn = C.FDefExt $ C.FunDef
      [C.TypeSpec C.Void]
      (C.Declr (Just $ CN.globalInitFn home name) [C.PtrDeclr [], C.FunDeclr []])
      revBody
  in
  bodyState
    { _revExtDecls = initFn : _revExtDecls bodyState
    , _revInitGlobals = global : _revInitGlobals bodyState
    }
    

generateFuncBody :: Opt.Global -> [Name.Name] -> Opt.Expr -> State -> (State, [C.CompoundBlockItem])
generateFuncBody global params elmExpr state =
  let
    (_, paramDestructDecls) =
      List.foldl'
        (\(index, blockItems) param ->
          ( index + 1
          , (C.BlockDecl $ C.Decl
              [C.TypeSpec C.Void]
              (Just $ C.Declr (Just $ CN.local param) [C.PtrDeclr []])
              (Just $ C.InitExpr $
                C.Index (C.Var CN.args) (C.Const $ C.IntConst index))
            ) : blockItems
          ))
        (0, [])
        params

    initExprState =
      CE.initState global paramDestructDecls (_revExtDecls state) (_sharedDefs state)

    (CE.ExprState cExpr revBlockItems revExtDecls sharedDefs _ _ _) =
      CE.generate initExprState elmExpr

    returnStmt =
      C.BlockStmt $ C.Return $ Just cExpr

    newState =
      state
        { _revExtDecls = revExtDecls
        , _sharedDefs = sharedDefs
        }
  in
    (newState, returnStmt : revBlockItems)
