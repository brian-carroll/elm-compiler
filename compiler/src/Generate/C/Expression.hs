{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
 ( generate
 , SharedDef(..)
 , HeaderMacro(..)
 , generateHeader
 , ExprState(..)
 , initState
 , generateEvalFn
 , globalDefsFromExprState
)
where

import Control.Monad.State (State, get, put, gets, modify)
import qualified Control.Monad.State as State


import qualified Data.ByteString.Builder as B
  -- import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Map ((!), Map)
import qualified Data.Map as Map
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
import qualified Data.Index as Index
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


-- Things that are globals in C but not in JS
data SharedDef
  = SharedInt Int
  | SharedFloat EF.Float
  | SharedChr ES.String
  | SharedStr ES.String
  | SharedAccessor N.Name
  | SharedFieldGroup [N.Name]
  | SharedJsThunk N.Name N.Name
  deriving (Eq, Ord)


data ExprState =
  ExprState
    { _revBlockItems :: [C.CompoundBlockItem]
    , _revExtDecls :: [C.ExternalDeclaration]
    , _sharedDefs :: Set SharedDef
    , _localScope :: Set N.Name
    , _freeVars :: Set N.Name
    , _tmpVarIndex :: Int
    , _parentGlobal :: Opt.Global
    }


initState :: Opt.Global -> [C.ExternalDeclaration] -> Set SharedDef -> ExprState
initState global revExtDecls sharedDefs =
  ExprState
    { _revBlockItems = []
    , _revExtDecls = revExtDecls
    , _sharedDefs = sharedDefs
    , _localScope = Set.empty
    , _freeVars = Set.empty
    , _tmpVarIndex = 0
    , _parentGlobal = global
    }


globalDefsFromExprState :: ExprState -> ([C.ExternalDeclaration], Set SharedDef)
globalDefsFromExprState state =
  ( _revExtDecls state
  , _sharedDefs state
  )


todo :: B.Builder -> State ExprState C.Expression
todo comment =
  return $ C.CommentExpr comment


addShared :: SharedDef -> State ExprState ()
addShared shared =
  State.modify (\state ->
    state { _sharedDefs = Set.insert shared (_sharedDefs state) }
  )


addSharedExpr :: SharedDef -> CN.Name -> State ExprState C.Expression
addSharedExpr shared name =
  do
    addShared shared
    return $ C.addrOf name
    

addBlockItem :: C.CompoundBlockItem -> State ExprState ()
addBlockItem blockItem =
  modify (\state ->
    state { _revBlockItems = blockItem : (_revBlockItems state) })


addLocal :: N.Name -> State ExprState ()
addLocal name =
  modify (\state ->
    state { _localScope = Set.insert name (_localScope state) })


nextTmpVarIndex :: State ExprState Int
nextTmpVarIndex =
  do
    state <- get
    let index = _tmpVarIndex state
    put $ state { _tmpVarIndex = index + 1 }
    return index


getTmpVarName :: State ExprState CN.Name
getTmpVarName =
  do
    index <- nextTmpVarIndex
    return $ CN.tmp index


startNewBlock :: State ExprState [C.CompoundBlockItem]
startNewBlock =
  do
    state <- get
    let oldBlockItems = _revBlockItems state
    put $ state { _revBlockItems = [] }
    return oldBlockItems


resumeBlock :: [C.CompoundBlockItem] -> State ExprState [C.CompoundBlockItem]
resumeBlock resumeBlockItems =
  do
    state <- get
    put $ state { _revBlockItems = resumeBlockItems }
    let exitingBlockItems = _revBlockItems state
    return exitingBlockItems


generate :: Opt.Expr -> State ExprState C.Expression
generate expr =
  case expr of
    Opt.Bool bool ->
      return $ C.addrOf $ if bool then CN.true else CN.false

    Opt.Chr char ->
      addSharedExpr (SharedChr char) (CN.literalChr char)

    Opt.Str string ->
      addSharedExpr (SharedStr string) (CN.literalStr string)

    Opt.Int int ->
      addSharedExpr (SharedInt int) (CN.literalInt int)

    Opt.Float float ->
      addSharedExpr (SharedFloat float) (CN.literalFloat float)

    Opt.VarLocal name ->
      do
        modify (\state ->
          if Set.member name (_localScope state) then
            state
          else
            state { _freeVars = Set.insert name (_freeVars state) })
        return $ C.Var $ CN.local name

    Opt.VarGlobal (Opt.Global home name) ->
      return $ C.addrOf $ CN.global home name

    Opt.VarEnum (Opt.Global home name) _ ->
      return $ C.addrOf $ CN.global home name

    Opt.VarBox (Opt.Global home name) ->
      return $ C.addrOf $ CN.global home name

    Opt.VarCycle home name ->
      return $ C.addrOf $ CN.cycleVar home name

    Opt.VarDebug name home _ _ ->
      return $ C.addrOf $ CN.global home name

    Opt.VarKernel home name ->
      addSharedExpr
        (SharedJsThunk home name)
        (CN.kernelValue home name)

    Opt.List entries ->
      generateList entries

    Opt.Function args body ->
      generateLocalFn args body

    Opt.Call func args ->
      generateCall func args

    Opt.TailCall name args ->
      todo "TailCall"

    Opt.If branches final ->
      generateIf branches final

    Opt.Let def body ->
      do
        generateDef def
        generate body

    Opt.Destruct (Opt.Destructor name path) body ->
      do
        generateDestruct name path
        generate body

    Opt.Case label root decider jumps ->
      todo "Case"

    Opt.Accessor field ->
      addSharedExpr
        (SharedAccessor field)
        (CN.accessor field)

    Opt.Access record field ->
      do
        recordExpr <- generate record
        return $ C.Call
          (C.Var CN.utilsAccessEval)
          [C.pointerArray
            [ C.castAsPtrTo C.Void $ C.Var $ CN.fieldId field
            , recordExpr
            ]]

    Opt.Update record fieldValueMap ->
      do
        cRecord <- generate record
        (cValues, nUpdates) <- generateChildren (Map.elems fieldValueMap)
        let fieldNames = Map.keys fieldValueMap
        let cFields = map (C.Var . CN.fieldId) fieldNames
        return $ C.Call
          (C.Var CN.utilsUpdate)
          [ cRecord
          , C.Const $ C.IntConst nUpdates
          , C.arrayLiteral (C.TypeDef CN.U32) cFields
          , C.pointerArray cValues
          ]

    Opt.Record fields ->
      generateRecord fields

    Opt.Unit ->
      return $ C.addrOf CN.unit

    Opt.Tuple a b maybeC ->
      generateTuple a b maybeC

    Opt.Shader src attributes uniforms ->
      todo "Shader"


generateChildren :: [Opt.Expr] -> State ExprState ([C.Expression], Int)
generateChildren elmChildren =
  foldr generateChildrenHelp (pure ([], 0)) elmChildren


generateChildrenHelp :: Opt.Expr
  -> State ExprState ([C.Expression], Int)
  -> State ExprState ([C.Expression], Int)
generateChildrenHelp elmChildExpr acc =
  do
    (children, nChildren) <- acc
    child <- generate elmChildExpr
    return (child : children, nChildren + 1)


-- LOCAL FUNCTION


generateLocalFn :: [N.Name] -> Opt.Expr -> State ExprState C.Expression
generateLocalFn params body =
  do
    (Opt.Global gHome gName) <- gets _parentGlobal
    tmpVarIndex <- nextTmpVarIndex
    let fname = CN.localEvaluator gHome gName tmpVarIndex
    freeVars <- generateEvalFn fname params body
    return $
      C.Call (C.Var $ CN.fromBuilder "NEW_CLOSURE")
        [ C.Const $ C.IntConst $ length freeVars
        , C.Const $ C.IntConst $ length freeVars + length params
        , C.Unary C.AddrOp $ C.Var fname
        , C.pointerArray (map (C.Var . CN.local) freeVars)
        ]


generateEvalFn :: CN.Name -> [N.Name] -> Opt.Expr -> State ExprState [N.Name]
generateEvalFn fname params body =
  do
    origState <- get
    put $ origState
      { _revBlockItems = []
      , _localScope = Set.fromList params
      , _freeVars = Set.empty
      }
    returnExpr <- generate body
    bodyState <- get

    let freeVarList = Set.toList (_freeVars bodyState)
    let extDecl = generateEvalFnDecl fname returnExpr
            (_revBlockItems bodyState) (freeVarList ++ params)

    -- If my child function refers to my parent's scope, I pass it down as a free var.
    let updatedOrigFreeVars = List.foldl'
          (\acc free ->
            if Set.member free (_localScope origState)
            then acc
            else Set.insert free acc)
          (_freeVars origState)
          freeVarList
    put $
      bodyState
        { _revExtDecls = extDecl : (_revExtDecls bodyState)
        , _revBlockItems = _revBlockItems origState
        , _localScope = _localScope origState
        , _freeVars = updatedOrigFreeVars
        }
    return freeVarList


generateEvalFnDecl :: CN.Name -> C.Expression -> [C.CompoundBlockItem] -> [N.Name] -> C.ExternalDeclaration
generateEvalFnDecl fname returnExpr blockItems params =
  let
    paramDecls =
      case params of
        [] -> []
        _ -> [C.argsArray]
  in
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Void]
    (C.Declr (Just fname) [C.PtrDeclr [], C.FunDeclr paramDecls])
    ( (C.BlockStmt $ C.Return $ Just returnExpr)
      : blockItems
      ++ (generateDestructParams params)
    )


generateDestructParams :: [N.Name] -> [C.CompoundBlockItem]
generateDestructParams params =
  let
    (_, revParamDecls) =
      List.foldl'
        (\(index, decls) param ->
          ( index + 1
          , (C.BlockDecl $ C.Decl
              [C.TypeSpec C.Void]
              (Just $ C.Declr (Just $ CN.local param) [C.PtrDeclr []])
              (Just $ C.InitExpr $
                C.Index (C.Var CN.args) (C.Const $ C.IntConst index))
            ) : decls
          ))
        (0, [])
        params
  in
  revParamDecls


-- RECORD


generateRecord :: Map N.Name Opt.Expr -> State ExprState C.Expression
generateRecord fields =
  let
    children = Map.elems fields
    fieldNames = Map.keys fields
    fieldGroupName = CN.fieldGroup fieldNames
  in
  do
    addShared (SharedFieldGroup fieldNames)
    (childExprs, nChildren) <- generateChildren children
    return $
      C.Call (C.Var $ CN.fromBuilder "NEW_RECORD")
        [ C.Unary C.AddrOp $ C.Var fieldGroupName
        , C.Const $ C.IntConst nChildren
        , C.pointerArray childExprs
        ]


generateTuple :: Opt.Expr -> Opt.Expr -> Maybe Opt.Expr -> State ExprState C.Expression
generateTuple a b maybeC =
  let
    (ctorName, children) =
      case maybeC of
        Nothing -> ( "NEW_TUPLE2", [a,b] )
        Just c -> ( "NEW_TUPLE3", [a,b,c] )
  in
  do
    (childExprs, nChildren) <- generateChildren children
    return $ C.Call (C.Var $ CN.fromBuilder ctorName) childExprs


generateIf :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> State ExprState C.Expression
generateIf branches finalElm =
  do
    tmpName <- getTmpVarName
    finalStmt <- generateIfBlock tmpName finalElm
    ifStmt <- foldr (generateIfBranch tmpName) (return finalStmt) branches
    addBlockItem $ C.BlockDecl $ C.Decl
      [C.TypeSpec C.Void]
      (Just $ C.Declr (Just tmpName) [C.PtrDeclr []])
      Nothing
    addBlockItem $ C.BlockStmt ifStmt
    return $ C.Var tmpName


generateIfBranch :: CN.Name -> (Opt.Expr, Opt.Expr)
  -> State ExprState C.Statement
  -> State ExprState C.Statement
generateIfBranch tmpName (condElm, thenElm) state =
  do
    elseStmt <- state
    condExpr <- generate condElm
    let condTest = C.Binary C.EqOp condExpr (C.Unary C.AddrOp $ C.Var CN.true)
    thenStmt <- generateIfBlock tmpName thenElm
    return $ C.If condTest thenStmt (Just elseStmt)


generateIfBlock :: CN.Name -> Opt.Expr -> State ExprState C.Statement
generateIfBlock tmpName expr =
  do
    outerBlock <- startNewBlock
    cExpr <- generate expr
    block <- resumeBlock outerBlock
    return $ C.Compound $
      (C.BlockStmt $ C.Expr $ Just $ C.Assign C.AssignOp (C.Var tmpName) cExpr)
      : block


generateDestruct :: N.Name -> Opt.Path -> State ExprState ()
generateDestruct name path =
  do
    addLocal name
    addBlockItem $ C.BlockDecl $
      C.Decl
        [C.TypeSpec C.Void]
        (Just $ C.Declr (Just $ CN.local name) [C.PtrDeclr []])
        (Just $ C.InitExpr $ generatePath path)


generatePath :: Opt.Path -> C.Expression
generatePath path =
  case path of
    Opt.Index index subPath ->
      C.Call (C.Var CN.utilsDestructIndex)
        [ generatePath subPath
        , C.Const $ C.IntConst $ Index.toMachine index
        ]

    Opt.Root name ->
      C.Var $ CN.local name 

    Opt.Field field subPath ->
      C.Call (C.Var CN.utilsAccessEval)
        [ C.pointerArray
          [ C.nameAsVoidPtr $ CN.fieldId field
          , generatePath subPath
          ]
        ]

    Opt.Unbox subPath ->
      -- ((Custom*)subPath)->values[0]
      C.Index
        (C.MemberArrow
          (C.Parens $ C.castAsPtrTo (C.TypeDef CN.Custom) (generatePath subPath))
          (CN.fromBuilder "values"))
        (C.Const $ C.IntConst 0)


generateList :: [Opt.Expr] -> State ExprState C.Expression
generateList entries =
  if List.null entries then
    return $ C.addrOf $ CN.fromBuilder "Nil"
  else
    do
      (cEntries, nEntries) <- generateChildren entries
      return $
        C.Call (C.Var CN.utilsListFromArray)
          [ C.Const $ C.IntConst nEntries
          , C.pointerArray cEntries
          ]


generateCall :: Opt.Expr -> [Opt.Expr] -> State ExprState C.Expression
generateCall func args =
  do
    (argExprs, nArgs) <- generateChildren args
    funcExpr <- generate func
    return $ C.Call (C.Var $ CN.applyMacro nArgs)
              (funcExpr : argExprs)


generateDef :: Opt.Def -> State ExprState ()
generateDef def =
  case def of
    Opt.Def name body ->
      do
        addLocal name
        bodyExpr <- generate body
        addBlockItem $
          C.BlockDecl $ C.Decl
            [C.TypeSpec C.Void]
            (Just $ C.Declr
              (Just $ CN.local name)
              [C.PtrDeclr []])
            (Just $ C.InitExpr bodyExpr)

    Opt.TailDef name argNames body ->
      undefined


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
