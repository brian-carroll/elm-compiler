{-# LANGUAGE OverloadedStrings #-}
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

import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CName
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

{-
int main() {
  GC_init();
  GC_register_root(&ptr_author_project_TestModule_curried);
  GC_register_root(&ptr_author_project_TestModule_main);
  init_author_project_TestModule_curried();
  init_author_project_TestModule_main();
  return ((ElmInt*)&author_project_TestModule_main)->value;
}
-}
generateCMain :: [Opt.Global] -> B.Builder
generateCMain initGlobals =
  let
    gcRegisterRoots = []

  in
  "int main() {}\n"


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
      addDeps deps state

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
  CName.toBuilder $
  CName.kernelHeaderFile home


generateKernelInclude :: B.Builder -> B.Builder
generateKernelInclude filename =
  "#include \"../kernel/" <> filename <> "\"\n"


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
      , CTypeSpec $ CTypeDef (CName.toIdent CName.typeFieldGroup) undefNode
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
            let ident = CName.toIdent $ CName.asField $ CName.fromLocal f in
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
