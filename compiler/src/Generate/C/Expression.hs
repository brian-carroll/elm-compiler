{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
 ( generate
 , SharedDef(..)
 , HeaderMacro(..)
 , generateHeader
 , castAsVoidPtr
-- , generateEvalFn
-- , generateConstClosure
-- , generateConstInt
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
import Data.Set (Set)
import qualified Data.Set as Set
-- import qualified Data.List as List
-- import Data.Map ((!))
-- import qualified Data.Map as Map
import qualified Data.Name as N
-- import qualified Data.Utf8 as Utf8

-- import qualified Generate.C.Builder as CB
import qualified Generate.C.Name as CN
import qualified Generate.C.AST as C

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt

import qualified Elm.Float as EF
import qualified Elm.String as ES

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
  = CExpr C.Expression
  | CBlock [C.CompoundBlockItem]


data SharedDef
  = SharedInt Int
  | SharedFloat EF.Float
  | SharedChr ES.String
  | SharedStr ES.String
  | SharedAccessor N.Name
  | SharedFieldGroup [N.Name]
  deriving (Eq, Ord)


data ExprState =
  ExprState
    { _code :: Code
    , _extDecls :: [C.ExternalDeclaration]
    , _sharedDefs :: Set SharedDef
    , _scope :: Set N.Name
    }


generate :: Opt.Expr -> C.Expression
generate expr =
  C.CommentExpr "Expression"
{-

  Very similar to the JS setup, only Record is different

  Bool        expr
  Chr         expr + literal
  Str         expr + literal
  Int         expr + literal
  Float       expr + literal
  VarLocal    expr
  VarGlobal   expr
  VarEnum     expr
  VarBox      expr
  VarCycle    expr (call)
  VarDebug    expr
  VarKernel   expr (not only top level rename, also core .elm files)
  List        expr
  Function    expr (Closure ref) & statement (alloc) & decl (closure) & extdecl (eval)
  Call        expr
  TailCall    statements (assignments to args[i])
  If          statement
  Let         decls
  Destruct    decls & exprs
  Case        statement
  Accessor    expr (Closure ref) & ext decl (Closure) - agh, this is another shared thing like literals
  Access      expr (call)
  Update      expr (call)
  Record      decl & statements & fieldGroup
  Unit        expr
  Tuple       expr (call)
  Shader


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
      Var $ Ident $ CN.toBuilder $ CN.local name

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


generateConstInt :: CN.Name -> Int -> Declaration
generateConstInt name value =
  let
    declSpecs = [TypeQual ConstQual, TypeSpec ElmInt]
    declarator = Declr (Just $ CN.toIdentAST name) []
    init = generateLiteralInt value
  in
  Decl declSpecs (Just declarator) (Just $ InitExpr init)


generateLiteralClosure :: Int -> Ident -> Expression
generateLiteralClosure maxValues evaluator =
  CompoundLit
    [ ( [MemberDesig $ Ident "header"] , InitExpr $ Call m_HEADER_CLOSURE [Const $ IntConst 0] )
    , ( [MemberDesig $ Ident "evaluator"] , InitExpr $ Unary AddrOp $ Var $ evaluator )
    , ( [MemberDesig $ Ident "max_values"] , InitExpr $ Const $ IntConst maxValues )
    ]


generateConstClosure :: CN.Name -> CN.Name -> Int -> Declaration
generateConstClosure closureName evalName maxValues =
  let
    declSpecs = [TypeQual ConstQual, TypeSpec Closure]
    declarator = Declr (Just $ CN.toIdentAST closureName) []
    init = generateLiteralClosure maxValues $ CN.toIdentAST evalName
  in
  Decl declSpecs (Just declarator) (Just $ InitExpr init)



generateEvalFn :: CN.Name -> [N.Name] -> Opt.Expr -> ExternalDeclaration
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
    (paramRenames ++ body)


generateParamRename :: Ident -> N.Name -> Int -> Declaration
generateParamRename argsArray name index =
  let
    id = CN.toIdentAST $ CN.local name
    declarator = Declr (Just id) [PtrDeclr []]
    init = Index (Var argsArray) (Const $ IntConst index)
  in
    Decl [TypeSpec Void] (Just declarator) (Just $ InitExpr init)
-}


castAsVoidPtr :: CN.Name -> C.Expression
castAsVoidPtr name =
  C.Cast
    (C.Decl [C.TypeSpec C.Void]
      (Just $ C.Declr Nothing [C.PtrDeclr []]) Nothing)
    (C.Var name)


data HeaderMacro
  = HEADER_INT
  | HEADER_FLOAT
  | HEADER_CHAR
  | HEADER_STRING Int
  | HEADER_LIST
  | HEADER_TUPLE2
  | HEADER_TUPLE3
  | HEADER_CUSTOM Int
  | HEADER_RECORD Int
  | HEADER_CLOSURE Int


generateHeader :: HeaderMacro -> C.Expression
generateHeader header =
  case header of
    HEADER_INT -> C.Var $ CN.fromBuilder "HEADER_INT"
    HEADER_FLOAT -> C.Var $ CN.fromBuilder "HEADER_FLOAT"
    HEADER_CHAR -> C.Var $ CN.fromBuilder "HEADER_CHAR"
    HEADER_STRING n -> C.Call (C.Var $ CN.fromBuilder "HEADER_STRING") [C.Const $ C.IntConst n]
    HEADER_LIST -> C.Var $ CN.fromBuilder "HEADER_LIST"
    HEADER_TUPLE2 -> C.Var $ CN.fromBuilder "HEADER_TUPLE2"
    HEADER_TUPLE3 -> C.Var $ CN.fromBuilder "HEADER_TUPLE3"
    HEADER_CUSTOM n -> C.Call (C.Var $ CN.fromBuilder "HEADER_CUSTOM") [C.Const $ C.IntConst n]
    HEADER_RECORD n -> C.Call (C.Var $ CN.fromBuilder "HEADER_RECORD") [C.Const $ C.IntConst n]
    HEADER_CLOSURE n -> C.Call (C.Var $ CN.fromBuilder "HEADER_CLOSURE") [C.Const $ C.IntConst n]
