{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
 ( generate
 , SharedDef(..)
 , HeaderMacro(..)
 , generateHeader
 , ExprState(..)
 , initState
 , generateEvalFn
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

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt

import qualified Elm.Float as EF
import qualified Elm.String as ES

import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Optimize.DecisionTree as DT

import Debug.Trace as Debug


-- Globals in C but not in Elm AST or in JS
data SharedDef
  = SharedInt Int
  | SharedFloat EF.Float
  | SharedChr ES.String
  | SharedStr ES.String
  | SharedAccessor N.Name
  | SharedFieldGroup [N.Name]
  | SharedField N.Name
  | SharedCtor N.Name
  | SharedJsKernel N.Name N.Name
  | SharedJsGlobal Opt.Global
  deriving (Eq, Ord)


-- STATE AND HELPERS


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


addBlockItems :: [C.CompoundBlockItem] -> State ExprState ()
addBlockItems revItems =
  modify (\state ->
    state {
      _revBlockItems = revItems ++ (_revBlockItems state)
    })


addExtDecl :: C.ExternalDeclaration -> State ExprState ()
addExtDecl extDecl =
  modify (\state ->
    state { _revExtDecls = extDecl : (_revExtDecls state) })


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



-- EXPRESSIONS


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
          (addShared $ SharedJsKernel home name)
        return $ C.addrOf $ CN.kernelValue home name

    Opt.List entries ->
      generateList entries

    Opt.Function args body ->
      generateLocalFn args body

    Opt.Call func args ->
      generateCall func args

    Opt.TailCall name args ->
      generateTailCall name args

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
      generateCase label root decider jumps

    Opt.Accessor field ->
      do
        addShared (SharedField field)
        addSharedExpr
          (SharedAccessor field)
          (CN.accessor field)

    Opt.Access record field ->
      do
        addShared (SharedField field)
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


generateLocalFn :: [N.Name] -> Opt.Expr -> State ExprState C.Expression
generateLocalFn params body =
  do    
    closureName <- getTmpVarName
    generateNamedLocalFn closureName params body
    return $ C.Var closureName


generateNamedLocalFn :: CN.Name -> [N.Name] -> Opt.Expr -> State ExprState ()
generateNamedLocalFn closureName params body =
  do
    (Opt.Global gHome gName) <- gets _parentGlobal
    evalIndex <- nextTmpVarIndex
    let evalName = CN.localEvaluator gHome gName evalIndex

    freeVars <- generateEvalFn evalName (Just params) body False
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


generateEvalFn :: CN.Name -> Maybe [N.Name] -> Opt.Expr -> Bool -> State ExprState [N.Name]
generateEvalFn fname maybeParams body isTailRec =
  do
    origState <- get
    put $ origState
      { _revBlockItems = []
      , _localScope = Set.fromList $ fromMaybe [] maybeParams
      , _freeVars = Set.empty
      }
    returnExpr <- generate body
    bodyState <- get

    let freeVarList = Set.toList (_freeVars bodyState)
    let closureParams = fmap (\ps -> freeVarList ++ ps) maybeParams
    let extDecl = generateEvalFnDecl fname returnExpr
            (_revBlockItems bodyState) closureParams isTailRec

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


generateCycleFn :: CN.Name -> CN.Name -> Opt.Expr -> State ExprState ()
generateCycleFn ptrName funcName elmExpr =
  do
    let ptr = C.Var ptrName
    addBlockItem $ C.BlockStmt $ C.If ptr (C.Return $ Just ptr) Nothing
    expr <- generate elmExpr
    addBlockItem $ C.BlockStmt $ C.Expr $ Just $
      C.Assign C.AssignOp ptr expr
    blockItems <- gets _revBlockItems
    addExtDecl $ generateEvalFnDecl funcName ptr blockItems (Just []) False


generateEvalFnDecl :: CN.Name -> C.Expression -> [C.CompoundBlockItem] -> Maybe [N.Name] -> Bool -> C.ExternalDeclaration
generateEvalFnDecl fname returnExpr blockItems maybeParams isTailRec =
  let
    paramDecls = 
      case maybeParams of
        Nothing -> []
        Just _ -> C.argsArray : gcTceData

    gcTceData =
      if isTailRec then
        [ C.Decl
            [C.TypeSpec C.Void]
            (Just $ C.Declr
              (Just CN.gcTceData)
              [C.PtrDeclr [], C.PtrDeclr []])
            Nothing
        ]
      else
        []

    tceGoto =
      if isTailRec then
        [ C.BlockStmt $ C.Label CN.tceLabel $ C.NullStatement ]
      else
        []
  in
  C.FDefExt $ C.FunDef
    [C.TypeSpec C.Void]
    (C.Declr (Just fname) [C.PtrDeclr [], C.FunDeclr paramDecls])
    ( (C.BlockStmt $ C.Return $ Just returnExpr)
      : blockItems
      ++ (generateDestructParams $ fromMaybe [] maybeParams)
      ++ tceGoto
    )


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
    addShared (SharedFieldGroup fieldNames)
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
    resultName <- getTmpVarName
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
    resultName <- getTmpVarName
    addBlockItem $ C.BlockDecl $ C.declare resultName Nothing
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
        let value = pathToCExpr root path
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
    testValue <- generateTestValue (pathToCExpr root path) test
    testExpr <- generateTest testValue test
    return $ testExpr : accExpr


generateTestValue :: C.Expression -> DT.Test -> State ExprState C.Expression
generateTestValue value test =
  case test of
    DT.IsCtor _ _ _ _ _ ->
      do
        testValName <- getTmpVarName
        addBlockItem $ C.BlockDecl $
          C.Decl
            [C.TypeSpec $ C.TypeDef CN.U32]
            (Just $ C.Declr (Just testValName) [])
            (Just $ C.InitExpr $
              C.MemberArrow
                (C.Parens $ C.castAsPtrTo (C.TypeDef CN.Custom) value)
                (CN.fromBuilder "ctor"))
        return $ C.Var testValName

    DT.IsInt _ ->
      do
        testValName <- getTmpVarName
        addBlockItem $ C.BlockDecl $
          C.Decl
            [C.TypeSpec $ C.TypeDef CN.I32]
            (Just $ C.Declr (Just testValName) [])
            (Just $ C.InitExpr $
              C.MemberArrow
                (C.Parens $ C.castAsPtrTo (C.TypeDef CN.ElmInt) value)
                (CN.fromBuilder "value"))
        return $ C.Var testValName

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
        addShared (SharedCtor name)
        return $ C.Binary C.EqOp
          value
          (C.Var $ CN.ctorId name)

    DT.IsBool bool ->
      return $ C.Binary C.EqOp value $ C.addrOf $
        if bool then CN.true else CN.false

    DT.IsInt int ->
      return $ C.Binary C.EqOp
        value
        (C.Const $ C.IntConst int)

    DT.IsChr char ->
      do
        addShared (SharedChr char)
        return $ C.Binary C.EqOp
          (C.Call
            (C.Var $ CN.applyMacro 2)
            [ C.addrOf CN.utilsEqual
            , value
            , (C.addrOf $ CN.literalChr char)
            ])
          (C.addrOf CN.true)

    DT.IsStr string ->
      do
        addShared (SharedStr string)
        return $ C.Binary C.EqOp
          (C.Call
            (C.Var $ CN.applyMacro 2)
            [ C.addrOf CN.utilsEqual
            , value
            , (C.addrOf $ CN.literalStr string)
            ])
          (C.addrOf CN.true)

    DT.IsCons ->
      return $ C.Binary C.NeqOp value (C.addrOf CN.nil)

    DT.IsNil ->
      return $ C.Binary C.EqOp value (C.addrOf CN.nil)

    DT.IsTuple ->
      error "COMPILER BUG - there should never be tests on a tuple"


pathToCExpr :: N.Name -> DT.Path -> C.Expression
pathToCExpr root path =
  case path of
    DT.IndexBuiltin index subPath ->
      -- ((Tuple3*)(subPath))->a
      C.MemberArrow
        (C.Parens $
          C.castAsPtrTo (C.TypeDef CN.Tuple3) $
          C.Parens (pathToCExpr root subPath))
        (CN.fromSmallIndex index)
    
    DT.IndexCustom index subPath ->
      -- ((Custom*)(subPath))->values[3]
      C.Index
        (C.MemberArrow
          (C.Parens $
            C.castAsPtrTo (C.TypeDef CN.Custom) $
            C.Parens (pathToCExpr root subPath))
          (CN.fromBuilder "values"))
        (C.Const $ C.IntConst $ Index.toMachine index)

    DT.Unbox subPath ->
      -- ((Custom*)(subPath))->values[0]
      C.Index
        (C.MemberArrow
          (C.Parens $
            C.castAsPtrTo (C.TypeDef CN.Custom) $
            C.Parens (pathToCExpr root subPath))
          (CN.fromBuilder "values"))
        (C.Const $ C.IntConst 0)

    DT.Empty ->
      C.Var $ CN.local root



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
generateTailCall name args =
  do
    let elmArgExprs = map snd args
    (cArgExprs, nArgs) <- generateChildren elmArgExprs
    tmpNames <- State.foldM generateTailCallArg [] cArgExprs

    generateTailCallGcAlloc nArgs
    State.foldM generateTailCallAssign (nArgs - 1) tmpNames
    addBlockItem $ C.BlockStmt $ C.Goto CN.tceLabel

    return $ C.Var CN.nullPtr


generateTailCallArg :: [CN.Name] -> C.Expression -> State ExprState [CN.Name]
generateTailCallArg tmpNames expr =
  do
    tmp <- getTmpVarName
    addBlockItem $ C.BlockDecl $ C.declare tmp $ Just expr
    return $ tmp : tmpNames


generateTailCallGcAlloc :: Int -> State ExprState ()
generateTailCallGcAlloc nArgs =
  addBlockItem $ C.BlockStmt $ C.Expr $ Just $
    C.Assign C.AssignOp
      (C.Unary C.DerefOp $ C.Var CN.gcTceData)
      (C.Call
        (C.Var CN.canThrowMacro)
        [C.Call
          (C.Var CN.gcTceIteration)
          [C.Const $ C.IntConst nArgs]
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
        generateNamedLocalFn (CN.local name) args body

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
    freeVars <- generateEvalFn tailFnName (Just argNames) body True
    let wrapperBody = C.Call (C.Var CN.gcTceEval)
                        [ C.addrOf tailFnName
                        , C.addrOf wrapFnName
                        , C.Const $ C.IntConst $ length argNames
                        , C.Var CN.args
                        ]
    addExtDecl $ generateEvalFnDecl wrapFnName wrapperBody [] (Just []) False
    return freeVars



-- C VALUE HEADERS


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
  | HEADER_FIELDGROUP Int
  | HEADER_CLOSURE Int


generateHeader :: HeaderMacro -> C.Expression
generateHeader header =
  let
    fixedSize macro =
      C.Var $ CN.fromBuilder macro
    varSize macro kids =
      C.Call
        (C.Var $ CN.fromBuilder macro)
        [C.Const $ C.IntConst kids]
  in
  case header of
    HEADER_INT -> fixedSize  "HEADER_INT"
    HEADER_FLOAT -> fixedSize "HEADER_FLOAT"
    HEADER_CHAR -> fixedSize "HEADER_CHAR"
    HEADER_STRING n -> varSize "HEADER_STRING" n
    HEADER_LIST -> fixedSize "HEADER_LIST"
    HEADER_TUPLE2 -> fixedSize "HEADER_TUPLE2"
    HEADER_TUPLE3 -> fixedSize "HEADER_TUPLE3"
    HEADER_CUSTOM n -> varSize "HEADER_CUSTOM" n
    HEADER_RECORD n -> varSize "HEADER_RECORD" n
    HEADER_FIELDGROUP n -> varSize "HEADER_FIELDGROUP" n
    HEADER_CLOSURE n -> varSize "HEADER_CLOSURE" n


traceBuilder :: B.Builder -> a -> a
traceBuilder builder thing =
  Debug.trace
    (show $ B.toLazyByteString builder)
    thing
