{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where
    
import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
-- import qualified Data.List as List
-- import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
-- import qualified Data.Utf8 as Utf8
import Language.C as C
import Language.C.Data.Name as C
import Language.C.Pretty as C
import qualified Text.PrettyPrint as PP

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
    , _revBuildersC :: [B.Builder]
    , _fieldGroupNames :: Map.Map [Name.Name] C.Ident
    , _ctors :: Set.Set Name.Name
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _revKernels = []
    , _revBuildersJS = []
    , _revBuildersC = []
    , _fieldGroupNames = Map.empty
    , _ctors = Set.empty
    }


-- Language.C AST uses unique integer IDs (confusingly called Name)
-- It's for fast equality, but we're not parsing so we don't need that
dummyNodeId :: C.Name
dummyNodeId =
  C.Name 0


identFromChars :: [Char] -> C.Ident
identFromChars chars =
  C.mkIdent C.nopos chars dummyNodeId


identWithPrefix :: [Char] -> Name.Name -> C.Ident
identWithPrefix prefix name =
  identFromChars (prefix ++ Name.toChars name)


identFromField :: Name.Name -> C.Ident
identFromField field =
  identWithPrefix "Field_" field


identFromCtor :: Name.Name -> C.Ident
identFromCtor field =
  identWithPrefix "Ctor" field


generate :: Opt.GlobalGraph -> Mains -> B.Builder
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    fieldGroup1 :: [Name.Name]
    fieldGroup1 =
      Map.keys $ Map.take 5 fieldFreqMap

    fieldGroup2 :: [Name.Name]
    fieldGroup2 =
      Map.keys $ Map.take 5 $ Map.drop 3 fieldFreqMap
  
    fieldGroupNames :: Map.Map [Name.Name] C.Ident
    fieldGroupNames = Map.fromList
      [ (fieldGroup1, identFromChars "fg1")
      , (fieldGroup2, identFromChars "fg2")
      ]

    ctors :: Set.Set Name.Name
    ctors =
      Set.map Name.fromChars $
        Set.fromList ["Red", "Green", "Blue"]

    state :: State
    state =
      emptyState
        { _fieldGroupNames = fieldGroupNames
        , _ctors = ctors
        }
    
    fieldEnumDecl :: CExtDecl
    fieldEnumDecl = predeclareFieldEnum fieldGroupNames

    fieldGroupDecls :: [CExtDecl]
    fieldGroupDecls = predeclareFieldGroups fieldGroupNames

    ctorEnumDecl :: CExtDecl
    ctorEnumDecl = predeclareCtorEnum ctors

    cFileContent :: CTranslUnit
    cFileContent =
      CTranslUnit
        (fieldEnumDecl : ctorEnumDecl : fieldGroupDecls)
        undefNode
  in
    B.stringUtf8 $ PP.render $ C.pretty cFileContent


predeclareEnum :: [Char] -> Set.Set C.Ident -> CExtDecl
predeclareEnum enumName memberIds =
  let
    enumMembers :: [(C.Ident, Maybe CExpr)]
    enumMembers =
      Set.foldr
        (\memberId idsAndVals -> (memberId, Nothing) : idsAndVals)
        []
        memberIds

    fieldEnum :: CEnum
    fieldEnum =
      CEnum
        (Just $ identFromChars enumName)
        (Just $ enumMembers)
        []
        undefNode

    typeSpecifier :: CTypeSpec
    typeSpecifier = CEnumType fieldEnum undefNode

    cDeclarationSpecifier :: CDeclSpec
    cDeclarationSpecifier = CTypeSpec typeSpecifier

    cDeclaration :: CDecl
    cDeclaration = CDecl [cDeclarationSpecifier] [] undefNode
  in
    CDeclExt cDeclaration

{-

const FieldGroup fg1 = {
  .size = 2,
  .fields = {Field_abc, Field_def}
};

-}
idFieldGroup :: C.Ident
idFieldGroup = (identFromChars "FieldGroup")

intLiteral :: Int -> CExpr
intLiteral i =
  CConst $ CIntConst (CInteger (fromIntegral i) C.DecRepr C.noFlags) undefNode


predeclareFieldGroup :: [Name.Name] -> C.Ident -> CExtDecl
predeclareFieldGroup fields fieldGroupName =
  let
    fieldGroupDeclSpec :: CDeclSpec
    fieldGroupDeclSpec =
      CTypeSpec $ CTypeDef idFieldGroup undefNode

    declarationSpecifiers :: [CDeclSpec]
    declarationSpecifiers =
      [ CTypeQual $ CConstQual undefNode
      , fieldGroupDeclSpec
      ]

    declarator :: CDeclr
    declarator =
      CDeclr (Just fieldGroupName) [] Nothing [] undefNode


    fieldInits :: CInitList
    fieldInits =
      zipWith
        (\f i ->
          ( [CMemberDesig (identFromChars "field") undefNode, CArrDesig (intLiteral i) undefNode]
          , CInitExpr (CVar (identFromField f) undefNode) undefNode
          )
        )
        fields
        [0..]

    exprFieldGroup :: CExpr
    exprFieldGroup =
      CCompoundLit
        (CDecl [fieldGroupDeclSpec] [] undefNode)
        ( ( [CMemberDesig (identFromChars "size") undefNode]
          , CInitExpr (CConst $ CIntConst (CInteger (fromIntegral $ length fields) C.DecRepr C.noFlags) undefNode) undefNode
          )
          : fieldInits 
        )
        undefNode

    initializer :: CInit
    initializer = CInitExpr exprFieldGroup undefNode

    declaration :: CDecl
    declaration = CDecl
      declarationSpecifiers
      [(Just declarator, Just initializer, Nothing)]
      undefNode
  in
    CDeclExt declaration
    

predeclareFieldGroups :: Map.Map [Name.Name] C.Ident -> [CExtDecl]
predeclareFieldGroups fieldGroups =
  Map.foldrWithKey
    (\k v acc -> predeclareFieldGroup k v : acc)
    []
    fieldGroups


predeclareCtorEnum :: Set.Set Name.Name -> CExtDecl
predeclareCtorEnum ctors =
  predeclareEnum "ElmCustomCtor" $
    Set.map identFromCtor ctors


predeclareFieldEnum :: Map.Map [Name.Name] C.Ident -> CExtDecl
predeclareFieldEnum fieldGroupNames =
  let    
    fieldNames :: Set.Set Name.Name
    fieldNames =
      Map.foldlWithKey
        (\acc fieldGroup _ -> Set.union acc $ Set.fromList fieldGroup)
        Set.empty
        fieldGroupNames

    fieldIds :: Set.Set C.Ident
    fieldIds = Set.map identFromField fieldNames
  in
    predeclareEnum "ElmRecordField" fieldIds
