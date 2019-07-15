{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where
    
import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
-- import Data.Monoid ((<>))
-- import qualified Data.List as List
-- import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Name as Name
-- import qualified Data.Set as Set
-- import qualified Data.Utf8 as Utf8

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
import qualified Language.C as C
import qualified Language.C.Data.Name as C
import qualified Language.C.Pretty as C
import qualified Text.PrettyPrint as PP




-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main


makeFieldIdent :: Name.Name -> C.Name -> C.Ident
makeFieldIdent field nodeName =
    C.mkIdent C.nopos ("RecField_" ++ Name.toChars field) nodeName


generate :: Opt.GlobalGraph -> Mains -> B.Builder
generate (Opt.GlobalGraph graph fields) mains =
  let
    -- Language.C requires each node to have a unique "name" which is actually an Int
    enumIdent :: C.Ident
    enumIdent =
      C.mkIdent C.nopos "ElmRecordField" (C.Name 0)

    fieldEnumIdents :: [C.Ident]
    fieldEnumIdents =
      zipWith makeFieldIdent (Map.keys fields) (C.namesStartingFrom 1)

    fieldEnum :: C.CEnumeration C.NodeInfo
    fieldEnum =
      C.CEnum
        (Just enumIdent)
        (Just $ map (\i -> (i, Nothing)) fieldEnumIdents)
        []
        C.undefNode

    prettyEnum :: String
    prettyEnum =
      PP.render $ C.pretty fieldEnum
  in
    B.stringUtf8 prettyEnum
