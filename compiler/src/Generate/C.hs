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
-- import qualified Generate.Mode as Mode
-- import qualified Reporting.Doc as D
-- import qualified Reporting.Render.Type as RT
-- import qualified Reporting.Render.Type.Localizer as L
import qualified Generate.C.FakeAST as FakeAST



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
    , _revJsKernels :: [B.Builder]
    , _jsKernelVars :: Set.Set (Name.Name, Name.Name)
    , _fieldGroups :: Set.Set [Name.Name]
    , _ctorNames :: Set.Set Name.Name
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _sharedDefs = Set.empty
    , _revInitGlobals = []
    , _revExtDecls = []
    , _revJsKernels = []
    , _jsKernelVars = Set.empty
    , _fieldGroups = Set.empty
    , _ctorNames = Set.empty
    }


generate :: Opt.GlobalGraph -> Mains -> B.Builder
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    -- state = Map.foldrWithKey (addMain graph) emptyState mains
    state = Map.foldrWithKey (addMain FakeAST.graph) emptyState FakeAST.mains
  in
    stateToBuilder state

-- TODO: put fromExtDecl inside the fold
stateToBuilder :: State -> B.Builder
stateToBuilder state =
  let
    initGlobals = _revInitGlobals state
    kernelNames = map CN.jsKernelValue $ Set.toList $ _jsKernelVars state
    ctorNames = map CN.ctorId $ Set.toList $ _ctorNames state
    fieldNames = map CN.fieldId $ Set.toList $
      Set.foldl' (List.foldl' $ flip Set.insert) Set.empty $
      _fieldGroups state
    sharedDefs = map generateSharedDef $ Set.toList $ _sharedDefs state
  in
  prependExtDecls [C.IncludeExt CN.KernelH] $
  prependExtDecls (generateEnum kernelNames) $
  prependExtDecls (generateEnum ctorNames) $
  prependExtDecls (generateEnum fieldNames) $
  prependExtDecls sharedDefs $
  prependExtDecls [generateFieldGroupArray (_fieldGroups state)] $
  prependExtDecls (_revExtDecls state) $
  prependExtDecls [generateCMain initGlobals] $
    ""


prependExtDecls :: [C.ExternalDeclaration] -> B.Builder -> B.Builder
prependExtDecls revExtDecls monolith =
  List.foldl' (\m ext -> (CB.fromExtDecl ext) <> m) monolith revExtDecls


{-
    Accumulated values
-}


generateCMain :: [Opt.Global] -> C.ExternalDeclaration
generateCMain initGlobals =
  let
    exitCode = CN.fromBuilder "exit_code"
    exitCodeDef = C.BlockDecl $ C.Decl [C.TypeSpec C.Int]
      (Just $ C.Declr (Just exitCode) [])
      (Just $ C.InitExpr $ C.Call (C.Var $ CN.fromBuilder "GC_init") [])
    earlyReturn = C.BlockStmt $ C.If (C.Var exitCode)
      (C.Return $ Just $ C.Var exitCode) Nothing
    initCalls = List.foldl' generateInitCall [] initGlobals
    regFG = C.Call (C.Var CN.wrapperRegisterFieldGroups) [C.Var CN.appFieldGroups]
  in
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Int]
    (C.Declr (Just $ CN.fromBuilder "main") [C.FunDeclr []])
    (C.Compound (
      [ exitCodeDef
      , earlyReturn
      ] ++
      initCalls ++
      [ C.BlockStmt $ C.Expr $ Just $ regFG
      , C.BlockStmt $ C.Return $ Just $ C.Var exitCode
      ]
    ))


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
      generateClosure (CN.accessor name) CN.utilsAccessEval
        2 [CE.castAsVoidPtr $ CN.fieldId name]

    CE.SharedFieldGroup names ->
      generateStructDef CN.FieldGroup (CN.fieldGroup names)
        [("size", C.Const $ C.IntConst $ length names)]
        (Just ("fields", map (C.Var . CN.fieldId) names))


generateClosure :: CN.Name -> CN.Name -> Int -> [C.Expression] -> C.ExternalDeclaration
generateClosure name evalName maxValues values =
  let nValues = length values
  in
  generateStructDef CN.Closure name
    [ ("header", CE.generateHeader $ CE.HEADER_CLOSURE nValues)
    , ("n_values", C.Const $ C.IntConst nValues)
    , ("max_values", C.Const $ C.IntConst maxValues)
    , ("evaluator",  C.Unary C.AddrOp $ C.Var evalName)
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



-- {-
--                 ELM 'MAIN' VALUES
-- -}

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
      state { _seenGlobals = Set.insert global seen }


addGlobalHelp :: Graph -> Opt.Global -> State -> State
addGlobalHelp graph global state =
  let
    addDeps deps someState =
      Set.foldl' (addGlobal graph) someState deps
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
      state

    Opt.Kernel chunks deps ->
      addDeps deps state
      -- state
    --  addKernel (addDeps deps state) $
    --    generateKernel global

    Opt.Enum index ->
      state

    Opt.Box ->
      state

    Opt.PortIncoming decoder deps ->
      addDeps deps state

    Opt.PortOutgoing encoder deps ->
      addDeps deps state


-- addKernel :: State -> B.Builder -> State
-- addKernel state kernel =
--   state { _revKernelsC = kernel : _revKernelsC state }


-- generateKernel :: Opt.Global -> B.Builder
-- generateKernel (Opt.Global home _) =
--   generateKernelInclude $
--   CN.toBuilder $
--   CN.kernelHeaderFile home


-- generateKernelInclude :: B.Builder -> B.Builder
-- generateKernelInclude filename =
--   "#include \"../kernel/" <> filename <> "\"\n"

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

    runtimeInit =
      generateInitFn global expr $
      addExtDecl
        (C.DefineExt globalName $ C.Parens $
          C.Unary C.DerefOp $ C.Var $ CN.globalInitPtr home' name')
        state
  in
  case expr of
    Opt.Function args body ->
      let
        evalFnName = CN.globalEvaluator home' name'
        arity = length args
        closure = generateClosure globalName evalFnName arity []
      in
      addExtDecl closure state

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
      -- TODO: decide if C or JS, generate either #define or Closure
      defineAlias (CN.cKernelValue home name) state

    -- impossible in global scope
    Opt.VarLocal _ -> undefined
    Opt.TailCall _ _ -> undefined


generateInitFn :: Opt.Global -> Opt.Expr -> State -> State
generateInitFn global@(Opt.Global home name) expr state =
  let
    initFn :: C.ExternalDeclaration
    initFn = C.FDefExt $ C.FunDef
      [C.TypeSpec C.Void]
      (C.Declr (Just $ CN.globalInitFn home name) [C.PtrDeclr [], C.FunDeclr []])
      (C.Compound [C.BlockStmt $ C.Return $ Just $ CE.generate expr])
  in
  state
    { _revExtDecls = initFn : _revExtDecls state
    , _revInitGlobals = global : _revInitGlobals state
    }


-- {-
--                 RECORD FIELDGROUP
-- -}

-- generateFieldGroup :: [Name.Name] -> C.Ident -> CExtDecl
-- generateFieldGroup fields fieldGroupName =
--   let
--     -- const FieldGroup
--     declarationSpecifiers :: [CDeclSpec]
--     declarationSpecifiers =
--       [ CTypeQual $ CConstQual undefNode
--       , CTypeSpec $ CTypeDef (CN.toIdent CN.typeFieldGroup) undefNode
--       ]

--     -- fg3
--     declarator :: CDeclr
--     declarator =
--       CDeclr (Just fieldGroupName) [] Nothing [] undefNode

--     -- {Field_aardvaark, Field_banana}
--     fieldsInitList :: CInitList
--     fieldsInitList =
--         map
--           (\f ->
--             let ident = CN.toIdent $ CN.asField $ CN.fromLocal f in
--             ([] , CInitExpr (CVar ident undefNode) undefNode))
--           fields
  
--     -- { 2, {Field_aardvaark, Field_banana} }
--     initializer :: CInit
--     initializer =
--       CInitList
--         [ ([], CInitExpr (CB.intLiteral $ length fields) undefNode)
--         , ([], CInitList fieldsInitList undefNode)
--         ]
--         undefNode

--     -- const FieldGroup fg3 = { 2, {Field_aardvaark, Field_banana} }
--     declaration :: CDecl
--     declaration = CDecl
--       declarationSpecifiers
--       [(Just declarator, Just initializer, Nothing)]
--       undefNode
--   in
--     CDeclExt declaration
