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
import qualified Language.C as C
import qualified Language.C.Data.Name as C
import qualified Language.C.Pretty as C
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
    , _fieldLists :: Set.Set [Name.Name]
    , _ctors :: Set.Set Name.Name
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _revKernels = []
    , _revBuildersJS = []
    , _revBuildersC = []
    , _fieldLists = Set.empty
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
    fieldLists :: Set.Set [Name.Name]
    fieldLists = Set.singleton $ Map.keys fieldFreqMap

    ctors :: Set.Set Name.Name
    ctors = Map.keysSet fieldFreqMap -- any old names will do for now

    state :: State
    state =
      emptyState
        { _fieldLists = fieldLists
        , _ctors = ctors
        }
    
    fieldEnumDecl :: C.CExternalDeclaration C.NodeInfo
    fieldEnumDecl =
      predeclareFieldEnum fieldLists

    ctorEnumDecl :: C.CExternalDeclaration C.NodeInfo
    ctorEnumDecl =
      predeclareCtorEnum ctors

    cFileContent :: C.CTranslUnit
    cFileContent =
      C.CTranslUnit (fieldEnumDecl : ctorEnumDecl : []) C.undefNode
  in
    B.stringUtf8 $ PP.render $ C.pretty cFileContent


predeclareFieldLists :: Set.Set [Name.Name] -> [C.CExternalDeclaration C.NodeInfo]
predeclareFieldLists fieldLists =
  []


predeclareEnum :: [Char] -> Set.Set C.Ident -> C.CExternalDeclaration C.NodeInfo
predeclareEnum enumName memberIds =
  let
    enumMembers :: [(C.Ident, Maybe (C.CExpression C.NodeInfo))]
    enumMembers =
      Set.foldr
        (\memberId idsAndVals -> (memberId, Nothing) : idsAndVals)
        []
        memberIds

    fieldEnum :: C.CEnumeration C.NodeInfo
    fieldEnum =
      C.CEnum
        (Just $ identFromChars enumName)
        (Just $ enumMembers)
        []
        C.undefNode

    typeSpecifier :: C.CTypeSpecifier C.NodeInfo
    typeSpecifier = C.CEnumType fieldEnum C.undefNode

    cDeclarationSpecifier :: C.CDeclarationSpecifier C.NodeInfo
    cDeclarationSpecifier = C.CTypeSpec typeSpecifier

    cDeclaration :: C.CDeclaration C.NodeInfo
    cDeclaration = C.CDecl [cDeclarationSpecifier] [] C.undefNode
  in
    C.CDeclExt cDeclaration


predeclareFieldEnum :: Set.Set [Name.Name] -> C.CExternalDeclaration C.NodeInfo
predeclareFieldEnum fieldLists =
  let    
    fieldNames :: Set.Set Name.Name
    fieldNames =
      Set.foldl
        (\acc fieldList -> Set.union acc $ Set.fromList fieldList)
        Set.empty
        fieldLists

    fieldIds :: Set.Set C.Ident
    fieldIds = Set.map identFromField fieldNames
  in
    predeclareEnum "ElmRecordField" fieldIds


predeclareCtorEnum :: Set.Set Name.Name -> C.CExternalDeclaration C.NodeInfo
predeclareCtorEnum ctors =
  predeclareEnum "ElmCustomCtor" $
    Set.map identFromCtor ctors
