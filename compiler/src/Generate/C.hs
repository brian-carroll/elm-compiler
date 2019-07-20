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
    , _fields :: Set.Set Name.Name
    , _ctors :: Set.Set Name.Name
    }


emptyState :: State
emptyState =
  State
    { _seenGlobals = Set.empty
    , _revKernels = []
    , _revBuildersJS = []
    , _revBuildersC = []
    , _fields = Set.empty
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


identFromFieldName :: Name.Name -> C.Ident
identFromFieldName field =
  identWithPrefix "Field_" field


generate :: Opt.GlobalGraph -> Mains -> B.Builder
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  let
    fields :: Set.Set Name.Name
    fields = Map.keysSet fieldFreqMap

    stateWithFields :: State
    stateWithFields = emptyState { _fields = fields }
  in
    generateFieldEnum stateWithFields


generateFieldEnum :: State -> B.Builder
generateFieldEnum state =
  let
    fields :: Set.Set Name.Name
    fields = _fields state

    identsAndValues :: [(C.Ident, Maybe (C.CExpression C.NodeInfo))]
    identsAndValues =
      Set.foldr
        (\f idsAndVals -> (identFromFieldName f, Nothing) : idsAndVals)
        []
        fields

    fieldEnum :: C.CEnumeration C.NodeInfo
    fieldEnum =
      C.CEnum
        (Just $ identFromChars "ElmRecordField")
        (Just $ identsAndValues)
        []
        C.undefNode

    prettyEnum :: String
    prettyEnum =
      PP.render $ C.pretty fieldEnum
  in
    B.stringUtf8 prettyEnum
