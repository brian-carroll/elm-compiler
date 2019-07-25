{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
( generate
-- , generateCtor
-- , generateField
-- , generateTailDef
-- , generateMain
-- , Code
-- , codeToExpr
-- , codeToStmtList
)
where


-- import qualified Data.IntMap as IntMap
-- import qualified Data.List as List
-- import Data.Map ((!))
-- import qualified Data.Map as Map
-- import qualified Data.Name as Name
-- import qualified Data.Set as Set
-- import qualified Data.Utf8 as Utf8
import Language.C as C
import Language.C.Data.Name as C
import Language.C.Pretty as C
import qualified Text.PrettyPrint as PP

import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CName
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
-- import qualified AST.Utils.Shader as Shader
-- import qualified Data.Index as Index
-- import qualified Elm.Compiler.Type as Type
-- import qualified Elm.Compiler.Type.Extract as Extract
-- import qualified Elm.Version as V
-- import qualified Elm.ModuleName as ModuleName
-- import qualified Elm.Package as Pkg
-- import qualified Json.Encode as Encode
-- import Json.Encode ((==>))
-- import qualified Optimize.DecisionTree as DT
-- import qualified Reporting.Annotation as A




data Code
  = Expr CExpr
  | Stat CStat


generate :: Int
generate = 0
