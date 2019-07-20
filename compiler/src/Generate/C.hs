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
import qualified Language.C as C
import qualified Language.C.Data.Name as C
import qualified Language.C.Pretty as C
import qualified Text.PrettyPrint as PP

-- import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Canonical as Can
-- import qualified Data.Index as Index
-- import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
-- import qualified Generate.JavaScript.Builder as JS
-- import qualified Generate.JavaScript.Expression as Expr
-- import qualified Generate.JavaScript.Functions as Functions
import qualified Generate.JavaScript.Name as JsName
-- import qualified Generate.Mode as Mode
-- import qualified Reporting.Doc as D
-- import qualified Reporting.Render.Type as RT
-- import qualified Reporting.Render.Type.Localizer as L
import qualified Elm.Compiler.Type.Extract as Extract



-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main


makeFieldIdent :: Name.Name -> C.Name -> C.Ident
makeFieldIdent field nodeName =
    C.mkIdent C.nopos ("RecField_" ++ Name.toChars field) nodeName


generate :: Opt.GlobalGraph -> Mains -> Extract.Types -> B.Builder
generate (Opt.GlobalGraph graph fields) mains types =
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

    prettyEnum :: B.Builder
    prettyEnum =
      (B.stringUtf8 $ PP.render $ C.pretty fieldEnum) <> "\n"

    -- Types
    -- (Extract.Types_ ) = types

  in
    -- prettyEnum
    Extract.toBuilder types

data State =
  State
    { _revKernels :: [B.Builder]
    , _revBuilders :: [B.Builder]
    , _seenGlobals :: Set.Set Opt.Global
    }

{-
addMain :: Extract.Types -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain types graph home _ state =
  addGlobal types graph state (Opt.Global home "main")

addGlobal :: Extract.Types -> Graph -> State -> Opt.Global -> State
addGlobal types graph state@(State revKernels builders seen) global =
  if Set.member global seen then
    state
  else
    addGlobalHelp types graph global $
      State revKernels builders (Set.insert global seen)


addGlobalHelp :: Extract.Types -> Graph -> Opt.Global -> State -> State
addGlobalHelp types graph global state =
  let
    addDeps deps someState =
      Set.foldl' (addGlobal mode graph) someState deps
  in
  case graph ! global of
    Opt.Define expr deps ->
      state
    Opt.DefineTailFunc argNames body deps ->
      state
    Opt.Ctor index arity ->
      state
    Opt.Link (Opt.Global moduleName name) ->
      state
    Opt.Cycle names values functions deps ->
      state
    Opt.Manager effectsType ->
      state
    Opt.Kernel chunks deps ->
      state
    Opt.Enum index ->
      state
    Opt.Box ->
      state
    Opt.PortIncoming decoder deps ->
      state
    Opt.PortOutgoing encoder deps ->
      state
-}


    -- map over modules
    -- map over names in the modules
    -- Extract.fromName gives me a Maybe Can.Annotation
    -- destructure to get Type
    -- pass that to showType
    -- print the full global name (JS format will do), its type and a newline

  -- foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b



{-
Generating Records
- need to declare special C runtime stuff
  - the field enum
  - the fieldsets
- given
  - all record types have a constructor function,
  which contains a Record literal
  - but in the AST they're just functions, not specially marked
- how?
  - need a Set of field names for the field enum
  - for the fieldset, just generate it immediately

- tricky
  - AST uses 'Access' for both Records and Custom
  - Both the same in JS (Objects), access via '.'
  - For me they are both structs too, but I can only access if 
    I declare the type - different from JS
  - => I'm going to have to keep a Map of my own C struct types
  - but even then I'm prob fucked
-}