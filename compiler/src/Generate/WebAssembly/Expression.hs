{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Expression
  ( ExprState
  , generate
  , initState
  , flushState
  , generateMemory
  , generateTable
  )
  where

  import Data.Word (Word8)
  import Data.Bits ((.&.), shiftR)
  import Data.Int (Int32)
  import Data.ByteString (ByteString)
  import qualified Data.ByteString.Builder as B
  import qualified Data.Binary.Put as Put
  import Data.Monoid ((<>))
  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Lazy as BSL
  import qualified Data.Text as Text
  import qualified Data.Text.Encoding as TE
  import qualified Data.Map.Strict as Map
  import qualified Data.List as List
  import qualified Data.Set as Set

  import qualified AST.Optimized as Opt
  import qualified Elm.Name as N
  import qualified AST.Module.Name as ModuleName
  import Generate.WebAssembly.AST
  import Generate.WebAssembly.Instructions
  import qualified Generate.WebAssembly.Builder as WAB
  import qualified Generate.WebAssembly.Identifier as Id

  import Debug.Trace as Debug


  -- EXPRESSION GENERATOR STATE  
  
  data ExprState =
    ExprState
      { revInstr :: [Instr]
      , revFunc :: [Declaration]
      , dataSegment :: B.Builder
      , dataStart :: Int32
      , dataEnd :: Int32
      , tableSize :: Int32
      , revTableFuncIds :: [FunctionId]
      , currentScope :: Scope
      }

  
  -- for debug
  instance Show ExprState where
    show state =
      concat $ List.intersperse ", "
        [ "tableSize: " ++ (show $ tableSize state)
        , "dataEnd: " ++ (show $ dataEnd state)
        ]


  data Scope =
    Scope
      { argNames :: Set.Set N.Name
      , localNames :: Set.Set N.Name
      , closedOverNames :: Set.Set N.Name
      }

  
  -- Initial state

  initState :: Int32 -> Int32 -> ExprState
  initState initDataOffset initTableSize =
    ExprState
      { revInstr = []
      , revFunc = []
      , dataSegment = ""
      , dataStart = initDataOffset
      , dataEnd = initDataOffset
      , tableSize = initTableSize
      , revTableFuncIds = []
      , currentScope = emptyScope
      }
  

  emptyScope :: Scope
  emptyScope =
    Scope
      { argNames = Set.empty
      , localNames = Set.empty
      , closedOverNames = Set.empty
      }


  -- Final state

  flushState :: B.Builder -> ExprState -> (Instr, [Declaration], ExprState)
  flushState uniqueId state =
    let
      -- Generated functions
      -- If we've created local vars, generate one more function
      (finalInstr, finalRevFunc) =
        case revInstr state of
          [] ->
            undefined -- can't have an empty expression

          [i] ->
            if Set.null topLevelLocals then
              ( i, revFunc state )
            else
              ( call thunkId []
              , (makeThunk [i]) : revFunc state
              )

          iList ->
            ( call thunkId []
            , (makeThunk $ reverse iList) : revFunc state
            )

      topLevelLocals = localNames $ currentScope state
      thunkId = FunctionName ("$start" <> uniqueId)
      makeThunk body =
        Function
          { _functionId = thunkId
          , _params = []
          , _locals = nameSetToLocalsList topLevelLocals
          , _resultType = Nothing
          , _body = body
          }

      -- Initialised memory
      dataDecls =
        if (dataStart state) == (dataEnd state) then
          []
        else
          [DataSegment (dataStart state) (dataSegment state)]

      -- Table elements
      (functionIds, elemOffset) =
        foldr
          (\fid (fids, offset) -> (fid : fids, offset - 1))
          ([], tableSize state)
          (revTableFuncIds state)

      elemDecls =
        case functionIds of
          [] -> []
          _ -> [ElementSegment elemOffset functionIds]
    in
      ( finalInstr
      , dataDecls ++ elemDecls ++ finalRevFunc
      , initState (dataEnd state) (tableSize state)
      )


  generateTable :: ExprState -> Table
  generateTable state =
    TableDeclaration (Limits (tableSize state) Nothing) AnyFunc


  generateMemory :: ExprState -> Memory
  generateMemory state =
    let
      initPages = 1 + (dataEnd state `quot` 65536)
      maxPages = Nothing
    in
      Memory MemIdxZero $ Limits initPages maxPages


  -- HELPERS

  addInstr :: ExprState -> Instr -> ExprState
  addInstr state instr =
    state { revInstr = instr : (revInstr state) }


  addDataLiteral :: ExprState -> Int32 -> ByteString -> ExprState
  addDataLiteral state ctor payload =
    let
      unescaped = (encodeInt32 ctor) <> payload
      offset = dataEnd state
    in
      state
        { dataSegment =
            (dataSegment state) <> escapeDataSegment unescaped
        , dataEnd =
            offset + (fromIntegral $ BS.length unescaped)
        , revInstr =
            (i32_const offset) : (revInstr state)
        }


  -- Escape bytes for writing between quotes in a UTF-8 WAT file
  escapeDataSegment :: ByteString -> B.Builder
  escapeDataSegment bytes =
    BS.foldl'
      (\builder byte ->
        let
          isControlChar = byte < 32
          isBackslash = byte == 92
          isDoublequote = byte == 34
        in
          if isControlChar || isBackslash || isDoublequote then
            builder <> "\\" <> B.word8HexFixed byte
          else
            builder <> B.word8 byte
      )
      ""
      bytes


  encodeText :: Text.Text -> ByteString
  encodeText text =
    TE.encodeUtf8 text


  encodeInt32 :: Int32 -> ByteString
  encodeInt32 i32 =
    BSL.toStrict $ Put.runPut $ Put.putInt32le i32


  encodeDouble :: Double -> ByteString
  encodeDouble d =
    BSL.toStrict $ Put.runPut $ Put.putDoublele d
  

  -- COMPARABLES

  data ComparableCtor
    = CompNil
    | CompCons
    | CompTuple2
    | CompTuple3
    | CompInt
    | CompFloat
    | CompChar
    | CompString


  comparableCtor :: ComparableCtor -> Int32
  comparableCtor ctor =
    case ctor of
      CompNil    -> 0
      CompCons   -> 1
      CompTuple2 -> 2
      CompTuple3 -> 3
      CompInt    -> 4
      CompFloat  -> 5
      CompChar   -> 6
      CompString -> 7


  -- EXPRESSION

  generate :: Opt.Expr -> ExprState -> ExprState
  generate expression untracedState =
    let
      state = Debug.trace (show untracedState) untracedState
    in
    case expression of
      Opt.Bool bool ->
        addInstr state $
          get_global $ Id.fromGlobal ModuleName.basics $ N.fromString $
          if bool then "True" else "False"
  
      Opt.Chr text ->
        addDataLiteral state
          (comparableCtor CompChar)
          (encodeText text)
  
      Opt.Str text ->
        let
          numCodePoints =
            encodeInt32 $ fromIntegral $ Text.length text
        in
          addDataLiteral state
            (comparableCtor CompString)
            (numCodePoints <> encodeText text)

      Opt.Int int ->
        addDataLiteral state
          (comparableCtor CompInt)
          (encodeInt32 $ fromIntegral int)
  
      Opt.Float double ->
        addDataLiteral state
          (comparableCtor CompFloat)
          (encodeDouble double)
  
      Opt.VarLocal name ->
        generateVarLocal name state
  
      Opt.VarGlobal (Opt.Global home name) ->
        addInstr state $
          get_global $ Id.fromGlobal home name
  
      Opt.VarEnum (Opt.Global home name) index ->
        addInstr state $
          get_global $ Id.fromGlobal home name
  
      Opt.VarBox (Opt.Global home name) ->
        addInstr state $
          get_global $ Id.fromGlobal ModuleName.basics N.identity
  
      Opt.VarCycle home name ->
        addInstr state $
          call_indirect
            (Id.fromFuncType [] I32)
            (get_global $ Id.fromCycle home name)
            []
  
      Opt.VarDebug name home region unhandledValueName ->
        state
        -- JsExpr $ generateDebug name home region unhandledValueName
  
      Opt.VarKernel home name ->
        state
        -- JsExpr $ JS.Ref (Name.fromKernel home name)
  
      Opt.List entries ->
        state
        -- case entries of
        --   [] ->
        --     JsExpr $ JS.Ref (Name.fromKernel N.list "Nil")
  
        --   _ ->
        --     JsExpr $
        --       JS.Call
        --         (JS.Ref (Name.fromKernel N.list "fromArray"))
        --         [ JS.Array $ map (generateJsExpr mode) entries
        --         ]

      Opt.Function args body ->
        generateFunction args body state
  
      Opt.Call func args ->
        generateCall func args state
  
      Opt.TailCall name args ->
        state
        -- JsBlock $ generateTailCall mode name args
  
      Opt.If branches final ->
        state
        -- generateIf mode branches final
  
      Opt.Let def body ->
        state
        -- JsBlock $
        --   generateDef mode def : codeToStmtList (generate mode body)
  
      Opt.Destruct (Opt.Destructor name path) body ->
        state
        -- let
        --   pathExpr = generatePath mode path
        --   pathDef = JS.Var [ (Name.fromLocal name, Just pathExpr) ]
        -- in
        -- JsBlock $ pathDef : codeToStmtList (generate mode body)
  
      Opt.Case label root decider jumps ->
        state
        -- JsBlock $ generateCase mode label root decider jumps
  
      Opt.Accessor field ->
        state
        -- JsExpr $ JS.Function Nothing [Name.dollar]
        --   [ JS.Return $ Just $
        --       JS.Access (JS.Ref Name.dollar) (generateField mode field)
        --   ]
  
      Opt.Access record field ->
        state
        -- JsExpr $ JS.Access (generateJsExpr mode record) (generateField mode field)
  
      Opt.Update record fields ->
        state
        -- JsExpr $
        --   JS.Call (JS.Ref (Name.fromKernel N.utils "update"))
        --     [ generateJsExpr mode record
        --     , generateRecord mode fields
        --     ]
  
      Opt.Record fields ->
        state
        -- JsExpr $ generateRecord mode fields
  
      Opt.Unit ->
        state
        -- case mode of
        --   Mode.Dev _ _ ->
        --     JsExpr $ JS.Ref (Name.fromKernel N.utils "Tuple0")
  
        --   Mode.Prod _ _ ->
        --     JsExpr $ JS.Int 0
  
      Opt.Tuple a b maybeC ->
        state
        -- JsExpr $
        --   case maybeC of
        --     Nothing ->
        --       JS.Call (JS.Ref (Name.fromKernel N.utils "Tuple2"))
        --         [ generateJsExpr mode a
        --         , generateJsExpr mode b
        --         ]
  
        --     Just c ->
        --       JS.Call (JS.Ref (Name.fromKernel N.utils "Tuple3"))
        --         [ generateJsExpr mode a
        --         , generateJsExpr mode b
        --         , generateJsExpr mode c
        --         ]
  
      Opt.Shader src ->
        state
        -- let string = JS.String (Text.encodeUtf8Builder src) in
        -- JsExpr $ JS.Object [ ( Name.fromLocal "src", string ) ]



  generateVarLocal :: N.Name -> ExprState -> ExprState
  generateVarLocal name state =
    addInstr
      (maybeInsertLocalClosedOver state name)
      (get_local $ Id.fromLocal name)
  

  maybeInsertLocalClosedOver :: ExprState -> N.Name -> ExprState
  maybeInsertLocalClosedOver state name =
    let
      scope = currentScope state

      isFromCurrentScope =
        Set.member name (argNames scope)
        || Set.member name (localNames scope)
    in
      if isFromCurrentScope then
        state
      else
        state
          { currentScope =
              scope
                { closedOverNames =
                    Set.insert name (closedOverNames scope)
                }
          }


  generateFunction :: [N.Name] -> Opt.Expr -> ExprState -> ExprState
  generateFunction args body state =
    let
      bodyState =
        generate body $
          state
            { revInstr = []
            , currentScope =
                Scope
                  { argNames = Set.fromList args
                  , localNames = Set.empty
                  , closedOverNames = Set.empty
                  }
            }
      
      bodyScope = currentScope bodyState

      closedOverSet = closedOverNames bodyScope
      
      tableOffset = tableSize bodyState

      funcId = FunctionName ("$elmFunc" <> B.int32Dec tableOffset)

      funcArgId = LocalIdx 0

      -- Closure data structure to implement 'first-class functions'
      (closureConstructCode, closureDestructCode) =
        generateClosure args closedOverSet closureLocalId funcArgId tableOffset

      funcLocals =
        concatMap nameSetToLocalsList $
          [ argNames bodyScope
          , localNames bodyScope
          , closedOverSet
          ]

      func =
        Function
          { _functionId = funcId
          , _params = [(funcArgId, I32)]
          , _locals = funcLocals
          , _resultType = Just I32
          , _body = closureDestructCode ++ (reverse $ revInstr bodyState)
          }


      -- Update surrounding scope where the function is created

      (closureLocalId, surroundingScope) =
        createTempVar "closure" $ currentScope state

      updatedSurroundingScope =
        addPassthruClosedOvers closedOverSet surroundingScope

      closureConstructBlock =
        block 
          (LabelName $ "$createClosure" <> B.int32Dec tableOffset)
          I32
          (closureConstructCode ++ [get_local closureLocalId])
    in
      bodyState
        { revInstr = closureConstructBlock : (revInstr state)
        , currentScope = updatedSurroundingScope
        , revFunc = func : (revFunc bodyState)
        , tableSize = 1 + tableSize bodyState
        , revTableFuncIds = funcId : revTableFuncIds bodyState
        }


  nameSetToLocalsList :: Set.Set N.Name -> [(LocalId, ValType)]
  nameSetToLocalsList nameSet =
    Set.foldr'
        (\name acc -> (Id.fromLocal name, I32) : acc)
        []
        nameSet


  createTempVar :: String -> Scope -> (LocalId, Scope)
  createTempVar name scope =
    let
      noElmClashPrefix = "$"
      uniqueSuffix = show (Set.size $ localNames scope)
      tempName =
        N.fromString $ noElmClashPrefix ++ name ++ uniqueSuffix
    in
      ( Id.fromLocal tempName
      , scope
          { localNames =
              Set.insert tempName (localNames scope)
          }
      )

  createTempVars :: [String] -> Scope -> ([LocalId], Scope)
  createTempVars names scope =
    List.foldl'
      (\(accIds, accScope) name ->
        let
          (thisId, nextScope) =
            createTempVar name accScope
        in
          (thisId : accIds, nextScope)
      )
      ([], scope)
      names          
          

  addPassthruClosedOvers :: Set.Set N.Name -> Scope -> Scope
  addPassthruClosedOvers closedOverSet surroundingScope =
    let
      allSurroundingNames =
        Set.unions
          [ argNames surroundingScope
          , localNames surroundingScope
          , closedOverNames surroundingScope
          ]

      passthruNames =
        Set.filter
          (\name -> not $ Set.member name allSurroundingNames)
          closedOverSet
    in
      surroundingScope
        { closedOverNames =
            Set.union (closedOverNames surroundingScope) passthruNames
        }


  generateClosure :: [N.Name] -> Set.Set N.Name -> LocalId -> LocalId -> Int32 -> ([Instr], [Instr])
  generateClosure args closedOverSet closureId funcArgId elemIdx =
    let
      nArgs = length args

      i32size = 4
      pointersSize = (nArgs + Set.size closedOverSet) * i32size
      elemIndexSize = i32size
      aritySize = i32size
      totalSize = elemIndexSize + aritySize + pointersSize

      createNewClosure =
        set_local closureId
          (call
            (_functionId gcAllocate)
            [i32_const $ fromIntegral totalSize]
          )

      storeElemIndex =
        i32_store 0 (get_local closureId) $
          (i32_const elemIdx)

      storeArity =
        i32_store 4 (get_local closureId) $
          (i32_const $ fromIntegral $ length args)

      (storeClosedOvers, destructClosedOvers) =
        generateClosedOverValues nArgs closedOverSet closureId funcArgId

      closureConstructCode =
        createNewClosure
        : storeElemIndex
        : storeArity
        : storeClosedOvers
      
      (closureDestructCode, _) =
        List.foldl'
          (\(argDestructCode, pointerIdx) name ->
            let
              byteOffset =
                (closureIndexToOffset pointerIdx)

              destructArg =
                generateClosureDestruct funcArgId name byteOffset
            in
            ( destructArg : argDestructCode
            , pointerIdx + 1
            )
          )
          (destructClosedOvers, 0)
          args
    in
      (closureConstructCode, closureDestructCode)


  closureIndexToOffset :: Int -> Int
  closureIndexToOffset pointerIdx =
    let
      pointerSize = 4
      headerSize = 8 -- elemIdx + arity
    in
      headerSize + (pointerSize * pointerIdx)

  
  generateClosedOverValues :: Int -> Set.Set N.Name -> LocalId -> LocalId -> ([Instr], [Instr])
  generateClosedOverValues nArgs closedOverSet closureId funcArgId =
    let
      -- Generate construction and destructuring code together,
      -- to guarantee offsets match
      (storeClosedOvers, destructClosedOvers, _) =
        Set.foldl'
          (\(insertCode, destructCode, pointerIdx) name ->
            let
              byteOffset =
                closureIndexToOffset pointerIdx

              insertInstr =
                generateClosureInsert closureId name byteOffset

              destructInstr =
                generateClosureDestruct funcArgId name byteOffset
            in
              ( insertInstr : insertCode
              , destructInstr : destructCode
              , pointerIdx + 1
              )
          )
          ([], [], nArgs)
          closedOverSet
    in
      (storeClosedOvers, destructClosedOvers)


  generateClosureInsert :: LocalId -> N.Name -> Int -> Instr
  generateClosureInsert closureId name byteOffset =
    i32_store byteOffset
      (get_local closureId)
      (get_local (Id.fromLocal name))


  generateClosureDestruct :: LocalId -> N.Name -> Int -> Instr
  generateClosureDestruct funcArgId name byteOffset =
    set_local (Id.fromLocal name) $
      i32_load byteOffset (get_local funcArgId)


  wrapResultInstr :: (Instr -> Instr) -> [Instr] -> [Instr]
  wrapResultInstr wrapper reverseInstructions =
    case reverseInstructions of
      resultInstr : rest ->
        (wrapper resultInstr) : rest
      [] ->
        undefined


  generateCall :: Opt.Expr -> [Opt.Expr] -> ExprState -> ExprState
  generateCall funcExpr args state =
    let
      funcState =
        generate funcExpr state

      ([closureLocalId, argPointerLocalId], updatedScope) =
        createTempVars ["closure", "arg"] (currentScope funcState)

      getClosureCopy :: Instr -> Instr
      getClosureCopy funcRefInstr =
        set_local closureLocalId $
          call (_functionId gcShallowCopy) [funcRefInstr]

      getArity =
        i32_load 4 $
          get_local closureLocalId

      getInitArgPointer =
        set_local argPointerLocalId $
          i32_add (get_local closureLocalId) $
          i32_add (i32_const 8) $
          i32_mul (i32_const 4) $
          getArity

      foldInitState =
        funcState
          { currentScope = updatedScope
          , revInstr =
              getInitArgPointer
              : (wrapResultInstr getClosureCopy (revInstr funcState))
          }

      argsInsertedState =
        List.foldl'
          (\accState argExpr ->
            let
              argState =
                generate argExpr accState
            in
              argState
                { revInstr =
                    wrapResultInstr
                      insertArg
                      (revInstr argState)
                }
          )
          foldInitState
          args

      insertArg :: Instr -> Instr
      insertArg argExpr =
        i32_store 0
          (tee_local argPointerLocalId
            (i32_sub
              (get_local argPointerLocalId)
              (i32_const 4)
            )
          )
          argExpr

      -- If we're now pointing at the lowest arg in the closure, it's full
      isClosureFull =
        (i32_eq (i32_const 8)
          (i32_sub
            (get_local argPointerLocalId)
            (get_local closureLocalId)
          )
        )

      funcTableIndex =
        i32_load 0 (get_local closureLocalId)

      evaluateBody =
        call_indirect elmFuncTypeId
          funcTableIndex
          [get_local closureLocalId]

      resultInstr =
        select
          (get_local closureLocalId)
          evaluateBody
          isClosureFull
    in
      argsInsertedState
        { revInstr = resultInstr : (revInstr argsInsertedState)
        }


  -- All Elm functions have the same type in Wasm, pointer -> pointer
  -- Take a pointer to the closure, return a pointer to the result
  elmFuncTypeId :: TypeId
  elmFuncTypeId =
    Id.fromFuncType [I32] I32


{-
  TODO
    - Test builder
      - Take some manually written WAT files
      - Manually translate to AST
      - Run builder

    - Test generator
      - manually write some Elm AST and generate WAT
        - read & debug
      - implement allocate and copy
        - ever-increasing heap with no actual GC
        - need size headers for values (change pointer arithmetic)
      - run it

    - Add manual Wat code
      - some kind of prelude
      - start function?
      - exports
-}


  gcAllocate :: Declaration
  gcAllocate =
    Function
      { _functionId = FunctionName "$gcAllocate"
      , _params = [(LocalName "$size", I32)]
      , _locals = []
      , _resultType = Just I32
      , _body = [unreachable] -- TODO
      }


  gcShallowCopy :: Declaration
  gcShallowCopy =
    Function
      { _functionId = FunctionName "$gcShallowCopy"
      , _params = [(LocalName "$from", I32)]
      , _locals = []
      , _resultType = Just I32
      , _body = [unreachable] -- TODO
      }