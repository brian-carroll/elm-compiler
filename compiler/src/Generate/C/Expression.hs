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


import qualified Data.ByteString.Builder as B
  -- import qualified Data.IntMap as IntMap
-- import qualified Data.List as List
-- import Data.Map ((!))
-- import qualified Data.Map as Map
import qualified Data.Name as Name
-- import qualified Data.Set as Set
-- import qualified Data.Utf8 as Utf8
-- import Language.C as C
-- import Language.C.Data.Name as C
-- import Language.C.Pretty as C
-- import qualified Text.PrettyPrint as PP

-- import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CN
import Generate.C.AST

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




-- data Code
--   = Expr CExpr
--   | Stat CStat


-- C Macros from Kernel
m_NEW_ELM_INT = Ident "NEW_ELM_INT"
m_A1 = Ident "A1"
m_A2 = Ident "A2"


generate :: Opt.Expr -> Expression
generate expr =
  case expr of
    Opt.Bool bool ->
      Unary AddrOp $ Var $ Ident $ if bool then "True" else "False"

    Opt.Chr char ->
      CommentExpr "Chr"

    Opt.Str string ->
      CommentExpr "Str"

    Opt.Int int ->
      Call (Var m_NEW_ELM_INT) [Const $ IntConst $ int]

    Opt.Float float ->
      CommentExpr "Float"

    Opt.VarLocal name ->
      Var $ Ident $ CN.toBuilder $ CN.fromLocal name

    Opt.VarGlobal (Opt.Global home name) ->
      Unary AddrOp $ Var $ Ident $ CN.toBuilder $ CN.fromGlobal home name

    Opt.VarEnum (Opt.Global home name) _ ->
      Unary AddrOp $ Var $ Ident $ CN.toBuilder $ CN.fromGlobal home name

    Opt.VarBox (Opt.Global home name) ->
      Unary AddrOp $ Var $ Ident $ CN.toBuilder $ CN.fromGlobal home name

    Opt.VarCycle home name ->
      Unary AddrOp $ Var $ Ident $ CN.toBuilder $ CN.fromGlobal home name

    Opt.VarDebug name home _ _ ->
      Unary AddrOp $ Var $ Ident $ CN.toBuilder $ CN.fromGlobal home name

    Opt.VarKernel home name ->
      Unary AddrOp $ Var $ Ident $ CN.toBuilder $ CN.fromKernel home name

    Opt.List entries ->
      CommentExpr "List"

    Opt.Function args body ->
      CommentExpr "Function"

    Opt.Call func args ->
      Call
        (Var $ Ident ("A" <> B.intDec (length args)))
        (generate func : map generate args)

    Opt.TailCall name args ->
      CommentExpr "TailCall"

    Opt.If branches final ->
      CommentExpr "If"

    Opt.Let def body ->
      CommentExpr "Let"

    Opt.Destruct (Opt.Destructor name path) body ->
      CommentExpr "Destruct"

    Opt.Case label root decider jumps ->
      CommentExpr "Case"

    Opt.Accessor field ->
      CommentExpr "Accessor"

    Opt.Access record field ->
      CommentExpr "Access"

    Opt.Update record fields ->
      CommentExpr "Update"

    Opt.Record fields ->
      CommentExpr "Record"

    Opt.Unit ->
      CommentExpr "Unit"

    Opt.Tuple a b maybeC ->
      CommentExpr "Tuple"

    Opt.Shader src attributes uniforms ->
      CommentExpr "Shader"


