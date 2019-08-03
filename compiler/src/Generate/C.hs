{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Generate.C
  ( generate
  )
  where

import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
-- import qualified Data.Utf8 as Utf8
import Language.C as C
import Language.C.Data.Name as C
import Language.C.Pretty as C
import qualified Text.PrettyPrint as PP
import Text.RawString.QQ (r)

import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CN
import qualified Generate.C.Expression as CE
import qualified Generate.C.AST as AST
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
    , _revKernelsJS :: [B.Builder]
    , _revBuildersJS :: [B.Builder]
    , _revKernelsC :: [B.Builder]
    , _revBuildersC :: [B.Builder]
    , _revInitGlobals :: [Opt.Global]
    , _seenFieldGroups :: Map.Map [Name.Name] Int
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _revKernelsJS = []
    , _revBuildersJS = []
    , _revKernelsC = cRequiredKernels
    , _revBuildersC = []
    , _revInitGlobals = []
    , _seenFieldGroups = Map.empty
    }


cRequiredKernels :: [B.Builder]
cRequiredKernels =
  map generateKernelInclude ["types.h", "gc.h"]


generate :: Opt.GlobalGraph -> Mains -> B.Builder
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    -- state = Map.foldrWithKey (addMain graph) emptyState mains
    state = Map.foldrWithKey (addMain FakeAST.graph) emptyState FakeAST.mains
  in
    stateToBuilder state


stateToBuilder :: State -> B.Builder
stateToBuilder state =
  prependBuilders (_revKernelsC state) $
    prependBuilders (_revBuildersC state) $
    generateCMain (_revInitGlobals state)


prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders


generateCMain :: [Opt.Global] -> B.Builder
generateCMain revInitGlobals =
  let
    globalNames =
      map (\(Opt.Global home name) -> CN.fromGlobal home name) revInitGlobals
    registrations = map
      (\g -> CB.nIndent1 <> "GC_register_root(&" <> (CN.toBuilder $ CN.globalInitPtr g) <> ");")
      globalNames
    inits = map
      (\g -> CB.nIndent1 <> (CN.toBuilder $ CN.globalInitFn g) <> "();")
      globalNames
    body =
      CB.nIndent1 <> "GC_init();" <>
      (prependBuilders registrations $
        prependBuilders inits $
        "\n")
  in
  "int main() {" <> body <> "}\n"


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
      addKernel (addDeps deps state) $
        generateKernel global

    Opt.Enum index ->
      state

    Opt.Box ->
      state

    Opt.PortIncoming decoder deps ->
      addDeps deps state

    Opt.PortOutgoing encoder deps ->
      addDeps deps state


addKernel :: State -> B.Builder -> State
addKernel state kernel =
  state { _revKernelsC = kernel : _revKernelsC state }


generateKernel :: Opt.Global -> B.Builder
generateKernel (Opt.Global home _) =
  generateKernelInclude $
  CN.toBuilder $
  CN.kernelHeaderFile home


generateKernelInclude :: B.Builder -> B.Builder
generateKernelInclude filename =
  "#include \"../kernel/" <> filename <> "\"\n"


{-
                GLOBAL DEFINITION
-}


addDef :: Opt.Global -> Opt.Expr -> State -> State
addDef global@(Opt.Global home' name') expr state =
  let
    globalName =
      CN.fromGlobal home' name'
    textMacro otherGlobal =
      defineTextMacro state globalName otherGlobal
    runtimeInit =
      defineRuntimeInit global expr state
  in
  case expr of
    Opt.Function args body ->
      let
        evalFnName = CN.evalFn globalName
        arity = length args -- TODO: add free variables
      in
      state {
        _revBuildersC =
          (CB.fromExtDecl $ AST.DeclExt $ CE.generateConstClosure globalName evalFnName arity)
          : (CB.fromExtDecl $ CE.generateEvalFn evalFnName args body)
          : _revBuildersC state
      }
    
    -- defineConst body

    Opt.Bool bool -> textMacro (if bool then CN.true else CN.false)
    Opt.Unit -> textMacro CN.unit
    Opt.VarGlobal (Opt.Global home name) -> textMacro (CN.fromGlobal home name)
    Opt.VarEnum (Opt.Global home name) _ -> textMacro (CN.fromGlobal home name)
    Opt.VarBox (Opt.Global home name) -> textMacro (CN.fromGlobal home name)
    Opt.VarCycle home name -> textMacro (CN.fromGlobal home name)
    Opt.VarDebug name home _ _ -> textMacro (CN.fromGlobal home name)
    Opt.VarKernel home name -> textMacro (CN.fromKernel home name)

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

    -- TODO: create these at compile time rather than runtime (global const)
    Opt.Chr _ -> runtimeInit
    Opt.Str _ -> runtimeInit
    Opt.Int _ -> runtimeInit
    Opt.Float _ -> runtimeInit
    Opt.Accessor _ -> runtimeInit

    -- impossible in global scope
    Opt.VarLocal _ -> undefined
    Opt.TailCall _ _ -> undefined


defineTextMacro :: State -> CN.CName -> CN.CName -> State
defineTextMacro state lvalue rvalue =
  state {
    _revBuildersC =
      ("#define " <> (CN.toBuilder lvalue) <> " " <> (CN.toBuilder rvalue) <> "\n")
      : _revBuildersC state
  }


defineRuntimeInit :: Opt.Global -> Opt.Expr -> State -> State
defineRuntimeInit global@(Opt.Global home name) expr state =
  let
    globalName = CN.fromGlobal home name
    g = CN.toBuilder globalName
    initPtr = CN.toBuilder $ CN.globalInitPtr globalName
    initFn = CN.toBuilder $ CN.globalInitFn globalName

    exprBuilder = CB.fromExpr $ CE.generate expr
        
    builder :: B.Builder
    builder =
      mconcat $ List.intersperse "\n"
        [ ""
        , "#define " <> g <> " (*" <> initPtr <> ")"
        , "ElmValue* " <> initPtr <> ";"
        , "void* " <> initFn <> "() {"
        , "    " <> initPtr <> " = " <> exprBuilder <> ";"
        , "    return NULL;"
        , "}"
        , ""
        ]
    -- TODO: init function could in theory throw a heap overflow, should catch it
  in
  state
    { _revBuildersC = builder : _revBuildersC state
    , _revInitGlobals = global : _revInitGlobals state
    }


{-
                RECORD FIELDGROUP
-}

generateFieldGroup :: [Name.Name] -> C.Ident -> CExtDecl
generateFieldGroup fields fieldGroupName =
  let
    -- const FieldGroup
    declarationSpecifiers :: [CDeclSpec]
    declarationSpecifiers =
      [ CTypeQual $ CConstQual undefNode
      , CTypeSpec $ CTypeDef (CN.toIdent CN.typeFieldGroup) undefNode
      ]

    -- fg3
    declarator :: CDeclr
    declarator =
      CDeclr (Just fieldGroupName) [] Nothing [] undefNode

    -- {Field_aardvaark, Field_banana}
    fieldsInitList :: CInitList
    fieldsInitList =
        map
          (\f ->
            let ident = CN.toIdent $ CN.asField $ CN.fromLocal f in
            ([] , CInitExpr (CVar ident undefNode) undefNode))
          fields
  
    -- { 2, {Field_aardvaark, Field_banana} }
    initializer :: CInit
    initializer =
      CInitList
        [ ([], CInitExpr (CB.intLiteral $ length fields) undefNode)
        , ([], CInitList fieldsInitList undefNode)
        ]
        undefNode

    -- const FieldGroup fg3 = { 2, {Field_aardvaark, Field_banana} }
    declaration :: CDecl
    declaration = CDecl
      declarationSpecifiers
      [(Just declarator, Just initializer, Nothing)]
      undefNode
  in
    CDeclExt declaration
