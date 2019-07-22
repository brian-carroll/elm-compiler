{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where

import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
-- import qualified Data.List as List
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
-- import qualified Generate.JavaScript.Builder as JS
-- import qualified Generate.JavaScript.Expression as Expr
-- import qualified Generate.JavaScript.Functions as Functions
-- import qualified Generate.JavaScript.Name as JsName
-- import qualified Generate.Mode as Mode
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
    , _revKernels :: [B.Builder]
    , _revBuildersJS :: [B.Builder]
    , _revGlobalsC :: [CExtDecl]
    , _revInitC :: [CBlockItem]
    , _fieldGroupNames :: Map.Map [Name.Name] C.Ident
    , _ctors :: Set.Set Name.Name
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _revKernels = []
    , _revBuildersJS = []
    , _revGlobalsC = []
    , _revInitC = []
    , _fieldGroupNames = Map.empty
    , _ctors = Set.empty
    }


generate :: Opt.GlobalGraph -> Mains -> B.Builder
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    state = Map.foldrWithKey (addMain graph) emptyState mains

    topLevelDecls :: [CExtDecl]
    topLevelDecls =
      (generateCtorEnum $ _ctors state)
      : (generateFieldEnum $ _fieldGroupNames state)
      : (generateFieldGroups $ _fieldGroupNames state)
      ++ (_revGlobalsC state)
      ++ [generateCMain $ _revInitC state]

    topLevelAST :: CTranslUnit
    topLevelAST =
      CTranslUnit topLevelDecls undefNode

    cFileContent :: String
    cFileContent =
      PP.render $ C.pretty topLevelAST
  in
    B.stringUtf8 $ cFileContent ++ "\n"


{-
                FINAL STATE
-}

generateCMain :: [CBlockItem] -> CExtDecl
generateCMain revInitStmts =
  let
    nonVariadic = False

    declarator :: CDeclarator NodeInfo
    declarator = CDeclr
      (Just $ CB.identFromChars "main")
      [CFunDeclr (Right ([], nonVariadic)) [] undefNode]
      Nothing
      []
      undefNode

    compoundStatement :: CStatement NodeInfo
    compoundStatement =
      let labels = [] in
      CCompound labels (reverse revInitStmts) undefNode

    emptyOldStyleDeclList = []
  in
  CFDefExt $ CFunDef
    [CB.voidDeclSpec]
    declarator
    emptyOldStyleDeclList
    compoundStatement
    undefNode


generateCtorEnum :: Set.Set Name.Name -> CExtDecl
generateCtorEnum ctors =
  CB.declareEnum "AppCustomCtor" CName.ctorPrefix ctors


generateFieldEnum :: Map.Map [Name.Name] C.Ident -> CExtDecl
generateFieldEnum fieldGroupNames =
  CB.declareEnum "AppRecordField" CName.fieldPrefix $
    Map.foldlWithKey
      (\acc fieldGroup _ -> Set.union acc $ Set.fromList fieldGroup)
      Set.empty
      fieldGroupNames


generateFieldGroups :: Map.Map [Name.Name] C.Ident -> [CExtDecl]
generateFieldGroups fieldGroups =
  Map.foldrWithKey
    (\k v acc -> generateFieldGroup k v : acc)
    []
    fieldGroups
      

generateFieldGroup :: [Name.Name] -> C.Ident -> CExtDecl
generateFieldGroup fields fieldGroupName =
  let
    -- const FieldGroup
    declarationSpecifiers :: [CDeclSpec]
    declarationSpecifiers =
      [ CTypeQual $ CConstQual undefNode
      , CTypeSpec $ CTypeDef CName.idFieldGroup undefNode
      ]

    -- fg3
    declarator :: CDeclr
    declarator =
      CDeclr (Just fieldGroupName) [] Nothing [] undefNode

    -- {Field_aardvaark, Field_banana}
    fieldsInitList :: CInitList
    fieldsInitList =
        map
          (\f -> ([], CInitExpr (CVar (CB.identWithPrefix CName.fieldPrefix f) undefNode) undefNode))
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
    Opt.Define expr deps -> state
    Opt.DefineTailFunc argNames body deps -> state
    Opt.Ctor index arity -> state
    Opt.Link (Opt.Global moduleName name) -> state
    Opt.Cycle names values functions deps -> state
    Opt.Manager effectsType -> state
    Opt.Kernel chunks deps -> state
    Opt.Enum index -> state
    Opt.Box -> state
    Opt.PortIncoming decoder deps -> state
    Opt.PortOutgoing encoder deps -> state