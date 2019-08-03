{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
( generate
, generateEvalFn
, generateConstClosure
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
m_NEW_ELM_INT = Var $ Ident "NEW_ELM_INT"
m_HEADER_CLOSURE = Var $ Ident "HEADER_CLOSURE"
m_HEADER_INT = Var $ Ident "HEADER_INT"


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
      Call (m_NEW_ELM_INT) [Const $ IntConst $ int]

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


generateLiteralInt :: Int -> Expression
generateLiteralInt value =
  CompoundLit
    [ ( [MemberDesig $ Ident "header"] , InitExpr m_HEADER_INT )
    , ( [MemberDesig $ Ident "value"] , InitExpr $ Const $ IntConst value )
    ]


generateLiteralClosure :: Int -> Ident -> Expression
generateLiteralClosure maxValues evaluator =
  CompoundLit
    [ ( [MemberDesig $ Ident "header"] , InitExpr $ Call m_HEADER_CLOSURE [Const $ IntConst 0] )
    , ( [MemberDesig $ Ident "evaluator"] , InitExpr $ Unary AddrOp $ Var $ evaluator )
    , ( [MemberDesig $ Ident "max_values"] , InitExpr $ Const $ IntConst maxValues )
    ]


generateConstClosure :: CN.CName -> CN.CName -> Int -> Declaration
generateConstClosure closureName evalName maxValues =
  let
    declSpecs = [TypeQual ConstQual, TypeSpec Closure]
    declarator = Declr (Just $ CN.toIdentAST closureName) []
    init = generateLiteralClosure maxValues $ CN.toIdentAST evalName
  in
  Decl declSpecs (Just declarator) (Just $ InitExpr init)



generateEvalFn :: CN.CName -> [Name.Name] -> Opt.Expr -> ExternalDeclaration
generateEvalFn fname params bodyExpr =
  let
    nparams = length params

    argsArray :: Ident
    argsArray = Ident "args"

    -- *args[2]
    argsArrayDeclarator :: Declarator
    argsArrayDeclarator =
      Declr (Just argsArray)
        [ ArrDeclr
            [ConstQual] -- TODO: needed?
            (ArrSize $ Const $ IntConst nparams)
        , PtrDeclr []
        ]

    -- void *args[2]
    argsArrayDeclaration :: Declaration
    argsArrayDeclaration =
      Decl
        [TypeSpec Void]
        (Just argsArrayDeclarator)  -- declarator (may be omitted)
        Nothing -- optional initialize

    -- *fname(void *args[2])
    funcDeclarator :: Declarator
    funcDeclarator =
      Declr
        (Just $ CN.toIdentAST fname)
        [ FunDeclr [argsArrayDeclaration]
        , PtrDeclr []
        ]

    paramRenames :: [CompoundBlockItem]
    paramRenames =
      zipWith
        (\name idx -> BlockDecl $ generateParamRename argsArray name idx)
        params [0..]
    
    body :: [CompoundBlockItem]
    body = [BlockStmt $ Return $ Just $ generate bodyExpr]
  in
  FDefExt $ FunDef
    [TypeSpec Void]
    funcDeclarator
    (Compound $ paramRenames ++ body)


generateParamRename :: Ident -> Name.Name -> Int -> Declaration
generateParamRename argsArray name index =
  let
    id = CN.toIdentAST $ CN.fromLocal name
    declarator = Declr (Just id) [PtrDeclr []]
    init = Index (Var argsArray) (Const $ IntConst index)
  in
    Decl [TypeSpec Void] (Just declarator) (Just $ InitExpr init)
