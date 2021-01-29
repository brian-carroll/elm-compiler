{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
 ( generate
 , ExprState
 , initState
 , generateEvalFunction
 , generateInitFunction
 , generateTailDefEval
 , generateCycleFn
 , globalDefsFromExprState
)
where

import Control.Monad (when)
import Control.Monad.State (State, get, put, gets, modify)
import qualified Control.Monad.State as State

import qualified Data.ByteString.Builder as B
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Name as N

import qualified Generate.C.Name as CN
import qualified Generate.C.AST as C
import qualified Generate.C.Kernel as CKernel
import qualified Generate.C.Literals as CL
import qualified Generate.C.Builder as CB

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt

import qualified Elm.Float as EF
import qualified Elm.String as ES

import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Optimize.DecisionTree as DT

import Debug.Trace as Debug


-- STATE AND HELPERS


data ExprState =
  ExprState
    { _revBlockItems :: [C.CompoundBlockItem]
    , _revExtDecls :: [C.ExternalDeclaration]
    , _literals :: CL.Literals
    , _localScope :: Set N.Name
    , _freeVars :: Set N.Name
    , _tmpVarIndex :: Int
    , _parentGlobal :: Opt.Global
    }


initState :: Opt.Global -> [C.ExternalDeclaration] -> CL.Literals -> ExprState
initState global revExtDecls literals =
  ExprState
    { _revBlockItems = []
    , _revExtDecls = revExtDecls
    , _literals = literals
    , _localScope = Set.empty
    , _freeVars = Set.empty
    , _tmpVarIndex = 0
    , _parentGlobal = global
    }


globalDefsFromExprState :: ExprState -> ([C.ExternalDeclaration], CL.Literals)
globalDefsFromExprState state =
  ( _revExtDecls state
  , _literals state
  )


addLiteral :: (a -> CL.Literals -> CL.Literals) -> a -> State ExprState ()
addLiteral insert value =
  State.modify (\state -> state { _literals = insert value (_literals state) })


addLiteralRef :: (a -> CL.Literals -> CL.Literals) -> (a -> CN.Name) -> a -> State ExprState C.Expression
addLiteralRef insert getName value =
  do
    addLiteral insert value
    return $ C.addrOf $ getName value


addBlockItem :: C.CompoundBlockItem -> State ExprState ()
addBlockItem blockItem =
  modify (\state ->
    state { _revBlockItems = blockItem : (_revBlockItems state) })


addBlockItems :: [C.CompoundBlockItem] -> State ExprState ()
addBlockItems revItems =
  modify (\state ->
    state {
      _revBlockItems = revItems ++ (_revBlockItems state)
    })


addCommentBlockItem :: B.Builder -> State ExprState ()
addCommentBlockItem comment =
  addBlockItem $ C.BlockStmt $ C.CommentStatement comment


addExtDecl :: C.ExternalDeclaration -> State ExprState ()
addExtDecl extDecl =
  modify (\state ->
    state { _revExtDecls = extDecl : (_revExtDecls state) })


addCommentExtDecl :: B.Builder -> State ExprState ()
addCommentExtDecl comment =
  addExtDecl $ C.CommentExt comment


addLocal :: N.Name -> State ExprState ()
addLocal name =
  modify (\state ->
    state { _localScope = Set.insert name (_localScope state) })


updateScope :: N.Name -> State ExprState ()
updateScope name =
  modify (\state ->
    if Set.member name (_localScope state) then
      state
    else
      state { _freeVars = Set.insert name (_freeVars state) })


nextTmpVarIndex :: State ExprState Int
nextTmpVarIndex =
  do
    state <- get
    let index = _tmpVarIndex state
    put $ state { _tmpVarIndex = index + 1 }
    return index


getTmpVarName :: B.Builder -> State ExprState CN.Name
getTmpVarName prefix =
  do
    index <- nextTmpVarIndex
    return $ CN.tmp prefix index


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



-- EXPRESSIONS


generate :: Opt.Expr -> State ExprState C.Expression
generate expr =
  case expr of
    Opt.Bool bool ->
      return $ C.addrOf $ if bool then CN.true else CN.false

    Opt.Chr char ->
      addLiteralRef CL.insertChr CN.literalChr char

    Opt.Str string ->
      addLiteralRef CL.insertStr CN.literalStr string

    Opt.Int int ->
      addLiteralRef CL.insertInt CN.literalInt int

    Opt.Float float ->
      addLiteralRef CL.insertFloat CN.literalFloat float

    Opt.VarLocal name ->
      do
        updateScope name
        return $ C.Var $ CN.local name

    Opt.VarGlobal (Opt.Global home name) ->
      return $ C.addrOf $ CN.global home name

    Opt.VarEnum (Opt.Global home name) _ ->
      return $ C.addrOf $ CN.global home name

    Opt.VarBox (Opt.Global home name) ->
      return $ C.addrOf $ CN.global home name

    Opt.VarCycle home name ->
      return $ C.Call (C.Var $ CN.cycleVar home name) []

    Opt.VarDebug name home _ _ ->
      return $ C.addrOf $ CN.global home name

    Opt.VarKernel home name ->
      do
        when (CKernel.shouldGenJsEnumId home name)
          (addLiteral CL.insertKernelJs (home, name))
        return $ C.addrOf $ CN.kernelValue home name

    Opt.List entries ->
      generateList entries

    Opt.Function args body ->
      do
        closureName <- getTmpVarName "f"
        generateLocalFn closureName args body
        return $ C.Var closureName

    Opt.Call func args ->
      generateCall func args

    Opt.TailCall name args ->
      generateTailCall name args

    Opt.If branches final ->
      generateIf branches final

    Opt.Let def body ->
      do
        letResultName <- getTmpVarName "let"
        addBlockItem $ C.declareVoidPtr letResultName Nothing
        outerBlock <- startNewBlock
        generateDef def
        cBodyExpr <- generate body
        let assignment = C.Assign C.AssignOp (C.Var letResultName) cBodyExpr
        addBlockItem $ C.BlockStmt $ C.Expr $ Just $ assignment
        innerBlock <- resumeBlock outerBlock
        addBlockItem $ C.BlockStmt $ C.Compound innerBlock
        return $ C.Var letResultName

    Opt.Destruct (Opt.Destructor name path) body ->
      do
        generateDestruct name path
        generate body

    Opt.Case label root decider jumps ->
      generateCase label root decider jumps

    Opt.Accessor field ->
      addLiteralRef CL.insertFieldAccessor CN.accessor field

    Opt.Access record field ->
      do
        addLiteral CL.insertFieldAccess field
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

    Opt.Shader _src _attributes _uniforms ->
      return $ C.CommentExpr "TODO: implement Shaders!" (C.Var CN.nullPtr)


generateChildren :: [Opt.Expr] -> State ExprState ([C.Expression], Int)
generateChildren elmChildren =
  foldr generateChildrenHelp (pure ([], 0)) elmChildren


generateChildrenHelp :: Opt.Expr
  -> State ExprState ([C.Expression], Int)
  -> State ExprState ([C.Expression], Int)
generateChildrenHelp elmChildExpr acc =
  do
    (children, nChildren) <- acc
    childExpr <- generate elmChildExpr
    childVarExpr <- generateChildVar childExpr
    return (childVarExpr : children, nChildren + 1)


generateChildVar :: C.Expression -> State ExprState C.Expression
generateChildVar childExpr =
  case childExpr of
    C.Var _ ->
      return childExpr

    C.Unary C.AddrOp (C.Var _) ->
      return childExpr

    _ ->
      do -- every child expression gets its own temp var for easier debugging
        childVarName <- getTmpVarName "tmp"
        addBlockItem $ C.declareVoidPtr childVarName (Just childExpr)
        return (C.Var childVarName)


generateExprAsBlock :: CN.Name -> Opt.Expr -> State ExprState [C.CompoundBlockItem]
generateExprAsBlock resultName expr =
  do
    outerBlock <- startNewBlock
    cExpr <- generate expr
    innerBlock <- resumeBlock outerBlock
    return $
      (C.BlockStmt $ C.Expr $ Just $ C.Assign C.AssignOp (C.Var resultName) cExpr)
      : innerBlock



-- LOCAL FUNCTION


generateLocalFn :: CN.Name -> [N.Name] -> Opt.Expr -> State ExprState ()
generateLocalFn closureName params body =
  do
    (Opt.Global gHome gName) <- gets _parentGlobal
    evalIndex <- nextTmpVarIndex
    let evalName = CN.localEvaluator gHome gName evalIndex

    freeVars <- generateEvalFunction evalName params body False
    addBlockItem $
      generateNewClosure closureName evalName (length params) (length freeVars)

    addBlockItems $ List.reverse $ map
      (generateFreeVarAssignment (C.Var closureName))
      (zip [0..] freeVars)


generateNewClosure :: CN.Name -> CN.Name -> Int -> Int -> C.CompoundBlockItem
generateNewClosure varName evalName nParams nFree =
  C.BlockDecl $
    C.Decl
      [C.TypeSpec $ C.TypeDef CN.Closure]
      (Just $ C.Declr (Just varName) [C.PtrDeclr []])
      (Just $ C.InitExpr $
        C.Call (C.Var $ CN.fromBuilder "NEW_CLOSURE")
          [ C.Const $ C.IntConst nFree
          , C.Const $ C.IntConst $ nFree + nParams
          , C.Unary C.AddrOp $ C.Var evalName
          , C.Var CN.nullPtr
          ])


generateFreeVarAssignment :: C.Expression -> (Int, N.Name) -> C.CompoundBlockItem
generateFreeVarAssignment closureRef (index, freeVarName) =
  C.BlockStmt $ C.Expr $ Just $
    C.Assign C.AssignOp
      (C.Index
        (C.MemberArrow closureRef $ CN.fromBuilder "values")
        (C.Const $ C.IntConst index))
      (C.Var $ CN.local freeVarName)


generateEvalFunction :: CN.Name -> [N.Name] -> Opt.Expr -> Bool -> State ExprState [N.Name]
generateEvalFunction fname params body isTailRec =
  do
    origState <- pushScope params
    returnExpr <- generate body
    bodyState <- get
    popScope origState bodyState
    let freeVarList = Set.toList (_freeVars bodyState)
    let declGenerator =
          if isTailRec
          then generateTailFnDecl
          else generateEvalFnDecl
    addExtDecl $ declGenerator fname returnExpr (_revBlockItems bodyState) (freeVarList ++ params)
    return freeVarList


pushScope :: [N.Name] -> State ExprState ExprState
pushScope params =
  do
    origState <- get
    put $ origState
      { _revBlockItems = []
      , _localScope = Set.fromList params
      , _freeVars = Set.empty
      }
    return origState


popScope :: ExprState -> ExprState -> State ExprState ()
popScope outerScopeState innerScopeState =
  let
    -- Any free vars not found in outer scope must come from some scope further out.
    -- These become free vars at all levels in-between, passed down as arguments in C.
    unresolvedFreeVars =
      Set.difference (_freeVars innerScopeState) (_localScope outerScopeState)
  in
  put $ innerScopeState
    { _revBlockItems = _revBlockItems outerScopeState
    , _localScope = _localScope outerScopeState
    , _freeVars = Set.union (_freeVars outerScopeState) unresolvedFreeVars
    }


generateEvalFnDecl :: CN.Name -> C.Expression -> [C.CompoundBlockItem] -> [N.Name] -> C.ExternalDeclaration
generateEvalFnDecl fname returnExpr blockItems params =
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Void]
    (C.Declr (Just fname) [C.PtrDeclr [], C.FunDeclr [C.argsArray]])
    ((C.BlockStmt $ C.Return $ Just returnExpr)
      : blockItems
      ++ (generateDestructParams params))


generateTailFnDecl :: CN.Name -> C.Expression -> [C.CompoundBlockItem] -> [N.Name] -> C.ExternalDeclaration
generateTailFnDecl fname returnExpr blockItems params =
  let
    paramDecls =
      [ C.argsArray
      , C.Decl
          [C.TypeSpec C.Void]
          (Just $ C.Declr
            (Just CN.gcTceData)
            [C.PtrDeclr [], C.PtrDeclr []])
          Nothing
      ]
  in
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Void]
    (C.Declr (Just fname) [C.PtrDeclr [], C.FunDeclr paramDecls])
    ( (C.BlockStmt $ C.Return $ Just returnExpr)
      : blockItems
      ++ (generateDestructParams params)
      ++ [ C.BlockStmt $ C.Label CN.tceLabel $ C.NullStatement ]
    )


generateInitFunction :: CN.Name -> Opt.Expr -> State ExprState ()
generateInitFunction fname body =
  do
    returnExpr <- generate body
    blockItems <- gets _revBlockItems
    addExtDecl $ generateNoArgsFunction fname blockItems returnExpr


generateCycleFn :: CN.Name -> CN.Name -> Opt.Expr -> State ExprState ()
generateCycleFn ptrName fname elmExpr =
  do
    let ptr = C.Var ptrName
    addBlockItem $ C.BlockStmt $ C.If ptr (C.Return $ Just ptr) Nothing
    expr <- generate elmExpr
    addBlockItem $ C.BlockStmt $ C.Expr $ Just $ C.Assign C.AssignOp ptr expr
    blockItems <- gets _revBlockItems
    addExtDecl $ generateNoArgsFunction fname blockItems ptr


generateNoArgsFunction :: CN.Name -> [C.CompoundBlockItem] -> C.Expression -> C.ExternalDeclaration
generateNoArgsFunction name blockItems returnExpr =
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Void]
    (C.Declr (Just name) [C.PtrDeclr [], C.FunDeclr []])
    ((C.BlockStmt $ C.Return $ Just returnExpr)
      : blockItems)


generateDestructParams :: [N.Name] -> [C.CompoundBlockItem]
generateDestructParams params =
  snd $ List.foldl'
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


-- RECORD


generateRecord :: Map N.Name Opt.Expr -> State ExprState C.Expression
generateRecord fields =
  let
    children = Map.elems fields
    fieldNames = Map.keys fields
    fieldGroupName = CN.fieldGroup fieldNames
  in
  do
    addLiteral CL.insertFieldGroup fieldNames
    (childExprs, nChildren) <- generateChildren children
    return $
      C.Call (C.Var $ CN.fromBuilder "NEW_RECORD")
        [ C.Unary C.AddrOp $ C.Var fieldGroupName
        , C.Const $ C.IntConst nChildren
        , C.pointerArray childExprs
        ]



-- TUPLE


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



-- IF EXPRESSION


generateIf :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> State ExprState C.Expression
generateIf branches finalElm =
  do
    resultName <- getTmpVarName "if"
    finalBlock <- generateExprAsBlock resultName finalElm
    ifStmt <- foldr
                (generateIfBranch resultName)
                (return $ C.Compound finalBlock)
                branches
    addBlockItem $ C.BlockDecl $ C.Decl
      [C.TypeSpec C.Void]
      (Just $ C.Declr (Just resultName) [C.PtrDeclr []])
      Nothing
    addBlockItem $ C.BlockStmt ifStmt
    return $ C.Var resultName


generateIfBranch :: CN.Name -> (Opt.Expr, Opt.Expr)
  -> State ExprState C.Statement
  -> State ExprState C.Statement
generateIfBranch resultName (condElm, thenElm) state =
  do
    elseStmt <- state
    condExpr <- generate condElm
    let condTest = C.Binary C.EqOp condExpr (C.Unary C.AddrOp $ C.Var CN.true)
    thenBlock <- generateExprAsBlock resultName thenElm
    return $ C.If condTest (C.Compound thenBlock) (Just elseStmt)



-- CASE EXPRESSIONS


generateCase :: N.Name -> N.Name -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> State ExprState C.Expression
generateCase label root decider jumps =
  do
    updateScope root
    resultName <- getTmpVarName "case"
    addBlockItem $ C.declareVoidPtr resultName Nothing
    defaultStmt <- generateDecider resultName label root decider
    stmts <- foldr
              (goto resultName label)
              (return [defaultStmt])
              jumps
    addBlockItem $ C.BlockStmt $
      C.DoWhile (C.Const $ C.IntConst 0) (C.Compound $ map C.BlockStmt stmts)
    return $ C.Var resultName


goto :: CN.Name -> N.Name -> (Int, Opt.Expr) -> State ExprState [C.Statement] -> State ExprState [C.Statement]
goto resultName label (index, branch) stmtsState =
  do
    stmts <- stmtsState
    branchExpr <- generate branch
    let branchStmt = C.Expr $ Just $ C.Assign C.AssignOp (C.Var resultName) branchExpr
    return $ (C.Label (CN.label label index) branchStmt) : stmts


generateDecider :: CN.Name -> N.Name -> N.Name -> Opt.Decider Opt.Choice -> State ExprState C.Statement
generateDecider resultName label root decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
      do
        block <- generateExprAsBlock resultName branch
        return $ C.Compound $ (C.BlockStmt C.Break) : block

    Opt.Leaf (Opt.Jump index) ->
      return $ C.Compound [C.BlockStmt $ C.Goto $ CN.label label index]

    Opt.Chain testChain success failure ->
      do
        testExprs <- foldr (generateTestChain root) (return []) testChain
        let chainExpr = List.foldl1' (C.Binary C.LandOp) testExprs
        successStmt <- generateDecider resultName label root success
        failureStmt <- generateDecider resultName label root failure
        return $ C.If chainExpr successStmt (Just failureStmt)

    Opt.FanOut path edges fallback ->
      do
        value <- generateTestPath root path
        testVal <- generateTestValue value $ fst (head edges)
        foldr
          (generateFanoutBranch resultName label root testVal)
          (generateDecider resultName label root fallback)
          edges


generateFanoutBranch :: CN.Name -> N.Name -> N.Name -> C.Expression -> (DT.Test, Opt.Decider Opt.Choice) -> State ExprState C.Statement -> State ExprState C.Statement
generateFanoutBranch resultName label root value (test, subTree) nextBranchState =
  do
    nextBranchStmt <- nextBranchState
    testExpr <- generateTest value test
    outerBlock <- startNewBlock
    subTreeStmt <- generateDecider resultName label root subTree
    innerBlock <- resumeBlock outerBlock
    let subTreeBlock = C.Compound ((C.BlockStmt subTreeStmt) : innerBlock)
    return $ C.If testExpr subTreeBlock (Just nextBranchStmt)


generateTestChain :: N.Name -> (DT.Path, DT.Test) -> State ExprState [C.Expression] -> State ExprState [C.Expression]
generateTestChain root (path, test) acc =
  do
    accExpr <- acc
    pathExpr <- generateTestPath root path
    testValue <- generateTestValue pathExpr test
    testExpr <- generateTest testValue test
    return $ testExpr : accExpr


generateTestValue :: C.Expression -> DT.Test -> State ExprState C.Expression
generateTestValue value test =
  case test of
    DT.IsCtor _ _ _ _ _ ->
      castUsingTmpVar (C.TypeDef CN.Custom) value

    DT.IsInt _ ->
      castUsingTmpVar (C.TypeDef CN.ElmInt) value

    DT.IsBool _ ->
      return value

    DT.IsChr _ ->
      return value

    DT.IsStr _ ->
      return value

    DT.IsCons ->
      return value

    DT.IsNil ->
      return value

    DT.IsTuple ->
      error "COMPILER BUG - there should never be tests on a tuple"


generateTest :: C.Expression -> DT.Test -> State ExprState C.Expression
generateTest value test =
  case test of
    DT.IsCtor _ name _ _ _ ->
      do
        addLiteral CL.insertCtor name
        let actualExpr = C.MemberArrow value (CN.fromBuilder "ctor")
        let expectedExpr = C.Var $ CN.ctorId name
        let testExpr = C.Binary C.EqOp actualExpr expectedExpr
        return $ C.Binary C.LandOp (nullCheck value) testExpr

    DT.IsBool bool ->
      return $ C.Binary C.EqOp value $ C.addrOf $
        if bool then CN.true else CN.false

    DT.IsInt int ->
      do
        let actualExpr = C.MemberArrow value (CN.fromBuilder "value")
        let expectedExpr = C.Const $ C.IntConst int
        let testExpr = C.Binary C.EqOp actualExpr expectedExpr
        return $ C.Binary C.LandOp (nullCheck value) testExpr

    DT.IsChr char ->
      do
        addLiteral CL.insertChr char
        let testExpr = C.Binary C.EqOp
              (C.Call
                (C.Var $ CN.applyMacro 2)
                [ C.addrOf CN.utilsEqual
                , value
                , (C.addrOf $ CN.literalChr char)
                ])
              (C.addrOf CN.true)
        return $ C.Binary C.LandOp (nullCheck value) testExpr

    DT.IsStr string ->
      do
        addLiteral CL.insertStr string
        let testExpr = C.Binary C.EqOp
              (C.Call
                (C.Var $ CN.applyMacro 2)
                [ C.addrOf CN.utilsEqual
                , value
                , (C.addrOf $ CN.literalStr string)
                ])
              (C.addrOf CN.true)
        return $ C.Binary C.LandOp (nullCheck value) testExpr

    DT.IsCons ->
      return $ C.Binary C.NeqOp value (C.addrOf CN.nil)

    DT.IsNil ->
      return $ C.Binary C.EqOp value (C.addrOf CN.nil)

    DT.IsTuple ->
      error "COMPILER BUG - there should never be tests on a tuple"


generateTestPath :: N.Name -> DT.Path -> State ExprState C.Expression
generateTestPath root path =
  case path of
    DT.IndexBuiltin index subPath ->
      do  -- Tuple2, Tuple3 or Cons. All have the same shape.
        subPathExpr <- generateTestPath root subPath
        tupleVar <- castUsingTmpVar (C.TypeDef CN.Tuple3) subPathExpr  -- Tuple3 is the most general
        let accessChildExpr = C.MemberArrow tupleVar (CN.fromSmallIndex index)
        let guardExpr = nullCheck tupleVar -- will be NULL if an outer nested pattern didn't match
        -- Don't need bounds check for builtins. Can only get different-sized variants for Custom.
        let safeAccessExpr = C.Cond guardExpr accessChildExpr (C.Var CN.nullPtr)
        childVarName <- getTmpVarName "path"
        addBlockItem $ C.declareVoidPtr childVarName (Just safeAccessExpr)
        return (C.Var childVarName)

    DT.IndexCustom index subPath ->
      generateTestPathCustom root index subPath

    DT.Unbox subPath ->
      generateTestPathCustom root Index.first subPath

    DT.Empty ->
      return $ C.Var $ CN.local root


generateTestPathCustom :: N.Name -> Index.ZeroBased -> DT.Path -> State ExprState C.Expression
generateTestPathCustom root index subPath =
  do
    subPathExpr <- generateTestPath root subPath
    customVar <- castUsingTmpVar (C.TypeDef CN.Custom) subPathExpr
    let indexInt = Index.toMachine index
    let minSize = 2 + (indexInt + 1)
    let accessChildExpr = C.Index
          (C.MemberArrow customVar (CN.fromBuilder "values"))
          (C.Const $ C.IntConst indexInt)
    let guardExpr = nullAndBoundsCheck minSize customVar
    let safeAccessExpr = C.Cond guardExpr accessChildExpr (C.Var CN.nullPtr)
    childVarName <- getTmpVarName "path"
    addBlockItem $ C.declareVoidPtr childVarName (Just safeAccessExpr)
    return (C.Var childVarName)


castUsingTmpVar :: C.TypeSpecifier -> C.Expression -> State ExprState C.Expression
castUsingTmpVar toType expr =
  do
    let prefix = "tmp" <> CB.fromTypeSpecifier toType
    tmp <- getTmpVarName prefix
    addBlockItem $ C.definePtr toType tmp (C.castAsPtrTo toType expr)
    return (C.Var tmp)


structBoundsCheck :: Int -> C.Expression -> C.Expression
structBoundsCheck minSize elmStructPtr =
  let
    header = C.MemberArrow elmStructPtr (CN.fromBuilder "header")
    size = C.MemberDot header (CN.fromBuilder "size")
  in
  C.Binary C.GeOp size (C.Const $ C.IntConst minSize)


nullCheck :: C.Expression -> C.Expression
nullCheck expr =
  C.Binary C.NeqOp expr (C.Var CN.nullPtr)


nullAndBoundsCheck :: Int -> C.Expression -> C.Expression
nullAndBoundsCheck minSize expr =
  C.Parens $ C.Binary C.LandOp (nullCheck expr) (structBoundsCheck minSize expr)


-- DESTRUCTURING


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
    Opt.IndexBuiltin index subPath ->
      -- ((Tuple3*)(subPath))->a
      C.MemberArrow
        (C.Parens $
          C.castAsPtrTo (C.TypeDef CN.Tuple3) $
          C.Parens (generatePath subPath))
        (CN.fromSmallIndex index)
    
    Opt.IndexCustom index subPath ->
      -- ((Custom*)(subPath))->values[3]
      C.Index
        (C.MemberArrow
          (C.Parens $
            C.castAsPtrTo (C.TypeDef CN.Custom) $
            C.Parens (generatePath subPath))
          (CN.fromBuilder "values"))
        (C.Const $ C.IntConst $ Index.toMachine index)

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
      -- ((Custom*)(subPath))->values[0]
      C.Index
        (C.MemberArrow
          (C.Parens $
            C.castAsPtrTo (C.TypeDef CN.Custom) $
            C.Parens (generatePath subPath))
          (CN.fromBuilder "values"))
        (C.Const $ C.IntConst 0)



-- LIST


generateList :: [Opt.Expr] -> State ExprState C.Expression
generateList entries =
  if List.null entries then
    return $ C.addrOf $ CN.fromBuilder "Nil"
  else
    do
      (cEntries, nEntries) <- generateChildren entries
      return $
        C.Call (C.Var CN.listCreate)
          [ C.Const $ C.IntConst nEntries
          , C.pointerArray cEntries
          ]



-- CALL


generateCall :: Opt.Expr -> [Opt.Expr] -> State ExprState C.Expression
generateCall func args =
  case func of
    Opt.VarGlobal global@(Opt.Global (ModuleName.Canonical pkg _) _) | pkg == Pkg.core ->
      generateCoreCall global args

    _ ->
      do
        (cArgs, nArgs) <- generateChildren args
        funcExpr <- generate func
        return $ C.Call (C.Var $ CN.applyMacro nArgs)
                  (funcExpr : cArgs)


generateGlobalCall :: ModuleName.Canonical -> N.Name -> [Opt.Expr] -> State ExprState C.Expression
generateGlobalCall home name args =
  do
    (cArgs, nArgs) <- generateChildren args
    return $ C.Call
      (C.Var $ CN.applyMacro nArgs)
      ((C.addrOf $ CN.global home name) : cArgs)


generateKernelCall :: N.Name -> N.Name -> [Opt.Expr] -> State ExprState C.Expression
generateKernelCall home name args =
  do
    (cArgs, nArgs) <- generateChildren args
    return $ C.Call
      (C.Var $ CN.applyMacro nArgs)
      ((C.addrOf $ CN.kernelValue home name) : cArgs)


generateCoreCall :: Opt.Global -> [Opt.Expr] -> State ExprState C.Expression
generateCoreCall (Opt.Global home@(ModuleName.Canonical _ moduleName) name) args =
  if moduleName == N.basics then
    generateBasicsCall home name args

  else
    generateGlobalCall home name args


generateBasicsCall :: ModuleName.Canonical -> N.Name -> [Opt.Expr] -> State ExprState C.Expression
generateBasicsCall home name args =
  case args of
    [elmArg] ->
      do
        arg <- generate elmArg
        case name of
          "not"      -> return $ C.Cond (C.Binary C.EqOp arg (C.addrOf CN.false)) (C.addrOf CN.true) (C.addrOf CN.false)
          "negate"   -> generateNegate elmArg
          _          -> generateGlobalCall home name args

    [elmLeft, elmRight] ->
      case name of
        "apL"      -> generate $ apply elmLeft elmRight
        "apR"      -> generate $ apply elmRight elmLeft
        _ ->
          generateGlobalCall home name args

    _ ->
      generateGlobalCall home name args


generateNegate :: Opt.Expr -> State ExprState C.Expression
generateNegate elmExpr =
  case elmExpr of
    Opt.Int x -> generate $ Opt.Int (-x)
    _ -> generateKernelCall N.basics "negate" [elmExpr]


apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor field ->
      Opt.Access value field

    Opt.Call f args ->
      Opt.Call f (args ++ [value])

    _ ->
      Opt.Call func [value]



-- TAILCALL


generateTailCall :: N.Name -> [(N.Name, Opt.Expr)] -> State ExprState C.Expression
generateTailCall name explicitArgs =
  do
    let elmArgExprs = map snd explicitArgs
    (cArgExprs, nExplicitArgs) <- generateChildren elmArgExprs
    tmpNames <- State.foldM generateTailCallArg [] cArgExprs

    freeVars <- gets _freeVars
    let nClosureValues = nExplicitArgs + (Set.size freeVars)

    generateTailCallGcAlloc nClosureValues
    State.foldM generateTailCallAssign (nClosureValues - 1) tmpNames
    addBlockItem $ C.BlockStmt $ C.Goto CN.tceLabel

    return $ C.Var CN.nullPtr


generateTailCallArg :: [CN.Name] -> C.Expression -> State ExprState [CN.Name]
generateTailCallArg tmpNames expr =
  do
    tmp <- getTmpVarName "tail"
    addBlockItem $ C.declareVoidPtr tmp $ Just expr
    return $ tmp : tmpNames


generateTailCallGcAlloc :: Int -> State ExprState ()
generateTailCallGcAlloc nValues =
  addBlockItem $ C.BlockStmt $ C.Expr $ Just $
    C.Assign C.AssignOp
      (C.Unary C.DerefOp $ C.Var CN.gcTceData)
      (C.Call
        (C.Var CN.canThrowMacro)
        [C.Call
          (C.Var CN.gcTceIteration)
          [C.Const $ C.IntConst nValues]
        ])


generateTailCallAssign :: Int -> CN.Name -> State ExprState Int
generateTailCallAssign argIdx tmpName =
  do
    addBlockItem $ C.BlockStmt $ C.Expr $ Just $
      C.Assign C.AssignOp
        (C.Index (C.Var CN.args) (C.Const $ C.IntConst argIdx))
        (C.Var tmpName)
    return (argIdx - 1)


-- LET DEFINITION


generateDef :: Opt.Def -> State ExprState ()
generateDef def =
  case def of
    Opt.Def name (Opt.Function args body) ->
      do
        addLocal name
        generateLocalFn (CN.local name) args body

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
      do
        -- names
        addLocal name
        tmpIndex <- nextTmpVarIndex
        (Opt.Global gHome gName) <- gets _parentGlobal
        let tailFnName = CN.localTailEvaluator gHome gName name
        let wrapFnName = CN.localEvaluator gHome gName tmpIndex

        freeVars <- generateTailDefEval tailFnName wrapFnName argNames body

        addBlockItem $
          C.BlockDecl $ C.Decl
            [C.TypeSpec C.Void]
            (Just $ C.Declr
              (Just $ CN.local name)
              [C.PtrDeclr []])
            (Just $ C.InitExpr $ C.Call (C.Var $ CN.fromBuilder "NEW_CLOSURE")
              [ C.Const $ C.IntConst $ length freeVars
              , C.Const $ C.IntConst $ length freeVars + length argNames
              , C.Unary C.AddrOp $ C.Var wrapFnName
              , C.pointerArray (map (C.Var . CN.local) freeVars)
              ])


generateTailDefEval :: CN.Name -> CN.Name -> [N.Name] -> Opt.Expr -> State ExprState [N.Name]
generateTailDefEval tailFnName wrapFnName argNames body =
  do
    freeVars <- generateEvalFunction tailFnName argNames body True
    let max_values = length argNames + length freeVars
    addExtDecl $ generateTailDefWrapperFn wrapFnName tailFnName max_values
    return freeVars


generateTailDefWrapperFn :: CN.Name -> CN.Name -> Int -> C.ExternalDeclaration
generateTailDefWrapperFn wrapFnName tailFnName max_values =
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Void]
    (C.Declr (Just wrapFnName) [C.PtrDeclr [], C.FunDeclr [C.argsArray]])
    [C.BlockStmt $ C.Return $ Just $
      C.Call (C.Var CN.gcTceEval)
        [ C.addrOf tailFnName
        , C.addrOf wrapFnName
        , C.Const $ C.IntConst max_values
        , C.Var CN.args
        ]
    ]









traceBuilder :: B.Builder -> a -> a
traceBuilder builder thing =
  Debug.trace
    (show $ B.toLazyByteString builder)
    thing
