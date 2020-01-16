{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Expression
 ( generate
 , SharedDef(..)
 , HeaderMacro(..)
 , generateHeader
 , ExprState(..)
 , initState
 , generateEvalFn
 , generateCycleFn
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
import qualified Elm.ModuleName as ModuleName
-- import qualified Elm.Package as Pkg
-- import qualified Json.Encode as Encode
-- import Json.Encode ((==>))
import qualified Optimize.DecisionTree as DT
-- import qualified Reporting.Annotation as A


-- Globals in C but not in Elm AST or in JS
data SharedDef
  = SharedInt Int
  | SharedFloat EF.Float
  | SharedChr ES.String
  | SharedStr ES.String
  | SharedAccessor N.Name
  | SharedFieldGroup [N.Name]
  | SharedJsThunk N.Name N.Name
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


addExtDecl :: C.ExternalDeclaration -> State ExprState ()
addExtDecl extDecl =
  modify (\state ->
    state { _revExtDecls = extDecl : (_revExtDecls state) })


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
      return $ C.Call (C.Var $ CN.cycleVar home name) []

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
      generateCase label root decider jumps

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
    (Opt.Global gHome gName) <- gets _parentGlobal
    tmpVarIndex <- nextTmpVarIndex
    let fname = CN.localEvaluator gHome gName tmpVarIndex
    freeVars <- generateEvalFn fname params body False
    return $
      C.Call (C.Var $ CN.fromBuilder "NEW_CLOSURE")
        [ C.Const $ C.IntConst $ length freeVars
        , C.Const $ C.IntConst $ length freeVars + length params
        , C.Unary C.AddrOp $ C.Var fname
        , C.pointerArray (map (C.Var . CN.local) freeVars)
        ]


generateEvalFn :: CN.Name -> [N.Name] -> Opt.Expr -> Bool -> State ExprState [N.Name]
generateEvalFn fname params body isTailRec =
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
            (_revBlockItems bodyState) (freeVarList ++ params) isTailRec

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
    addExtDecl $ generateEvalFnDecl funcName ptr blockItems [] False


generateEvalFnDecl :: CN.Name -> C.Expression -> [C.CompoundBlockItem] -> [N.Name] -> Bool -> C.ExternalDeclaration
generateEvalFnDecl fname returnExpr blockItems params isTailRec =
  let
    paramDecls =
      case params of
        [] -> []
        _ -> C.argsArray : gcTceData

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
      ++ (generateDestructParams params)
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
    resultName <- getTmpVarName
    addBlockItem $ C.BlockDecl $ C.declare resultName Nothing
    defaultStmt <- generateDecider resultName label root decider
    stmts <- foldr
              (goto resultName label)
              (return [defaultStmt]) -- two block items, including decl for result
              jumps
    modify (\state ->
      state { _revBlockItems = (map C.BlockStmt stmts) ++ (_revBlockItems state) })
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
        return $ C.Compound block

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
    DT.IsCtor home name _ _ opts ->
      do
        testValName <- getTmpVarName
        addBlockItem $ C.BlockDecl $ C.declare testValName $ Just $
          C.MemberArrow
            (C.Parens $ C.castAsPtrTo (C.TypeDef CN.Custom) value)
            (CN.fromBuilder "ctor")
        return $ C.Var testValName

    DT.IsInt int ->
      do
        testValName <- getTmpVarName
        addBlockItem $ C.BlockDecl $ C.declare testValName $ Just $
          C.MemberArrow
            (C.Parens $ C.castAsPtrTo (C.TypeDef CN.ElmInt) value)
            (CN.fromBuilder "value")
        return $ C.Var testValName
    
    DT.IsBool bool ->
      return value

    DT.IsChr char ->
      return value

    DT.IsStr string ->
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
    DT.IsCtor home name _ _ opts ->
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
            [ C.Var CN.utilsEqual
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
            [ C.Var CN.utilsEqual
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
    DT.Index index subPath ->
      C.Call (C.Var CN.utilsDestructIndex)
        [ pathToCExpr root subPath
        , C.Const $ C.IntConst $ Index.toMachine index
        ]

    DT.Unbox subPath ->
      -- ((Custom*)subPath)->values[0]
      C.Index
        (C.MemberArrow
          (C.Parens $ C.castAsPtrTo (C.TypeDef CN.Custom) (pathToCExpr root subPath))
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



-- LIST


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



-- CALL


generateCall :: Opt.Expr -> [Opt.Expr] -> State ExprState C.Expression
generateCall func args =
  do
    (argExprs, nArgs) <- generateChildren args
    funcExpr <- generate func
    return $ C.Call (C.Var $ CN.applyMacro nArgs)
              (funcExpr : argExprs)



-- LET DEFINITION


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
      do
        -- names
        addLocal name
        tmpIndex <- nextTmpVarIndex
        (Opt.Global gHome gName) <- gets _parentGlobal
        let tailFnName = CN.localTailEvaluator gHome gName name
        let wrapFnName = CN.localEvaluator gHome gName tmpIndex

        freeVars <- generateEvalFn tailFnName argNames body True

        let wrapperBody = C.Call (C.Var CN.gcTceEval)
                            [ C.addrOf tailFnName
                            , C.addrOf wrapFnName
                            , C.Const $ C.IntConst $ length argNames
                            , C.Var CN.args
                            ]
        addExtDecl $ generateEvalFnDecl wrapFnName wrapperBody [] [] False

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
    HEADER_CLOSURE n -> varSize "HEADER_CLOSURE" n
