{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Expression where

  import Data.Word (Word8)
  import Data.Bits ((.&.), shiftR)
  import Data.Int (Int32)
  import Data.ByteString (ByteString)
  import qualified Data.ByteString.Builder as B
  import Data.Monoid ((<>))
  import qualified Data.ByteString as BS
  import qualified Data.Text as Text
  import qualified Data.Text.Encoding as TE
  import qualified Data.Map.Strict as Map
  import qualified Data.List as List
  import qualified Data.Set as Set

  import qualified AST.Optimized as Opt
  import Generate.WebAssembly.AST
  import Generate.WebAssembly.Instructions
  import Generate.WebAssembly.Builder as WA
  import qualified Elm.Name as N
  import qualified Generate.WebAssembly.Identifier as Identifier
  import qualified AST.Module.Name as ModuleName


  -- EXPRESSION GENERATOR STATE  
  
  data ExprState =
    ExprState
      { revInstr :: [Instr]
      , revFunc :: [Function]
      , dataSegment :: B.Builder
      , dataOffset :: Int32
      , tableSize :: Int32
      , revTableFuncIds :: [FunctionId]
      , revStartInstr :: [Instr]
      , currentScope :: Scope
      }


  data Scope =
    Scope
      { argNames :: Set.Set N.Name
      , localNames :: Set.Set N.Name
      , closedOverNames :: Set.Set N.Name
      }


  addInstr :: Instr -> ExprState -> ExprState
  addInstr instr state =
    state { revInstr = instr : (revInstr state) }

  addInstrList :: [Instr] -> ExprState -> ExprState
  addInstrList instrs state =
    state { revInstr = (List.reverse instrs) ++ (revInstr state) }
    

  addStartInstr :: Instr -> ExprState -> ExprState
  addStartInstr instr state =
    state { revStartInstr = instr : (revStartInstr state) }
  

  addFunc :: Function -> ExprState -> ExprState
  addFunc func state =
    state { revFunc = func : (revFunc state) }


  addTableFunc :: Function -> ExprState -> ExprState
  addTableFunc func state =
    state
      { revFunc = func : (revFunc state)
      , revTableFuncIds = (_functionId func) : (revTableFuncIds state)
      , tableSize = tableSize state + 1
      }


  addData :: ByteString -> ExprState -> ExprState
  addData bytes state =
    state
      -- dataSegment is escaped for writing to a file.
      -- Browser will un-escape when initialising Wasm memory.
      { dataSegment =
          (dataSegment state) <> bytesToBuilderEscaped bytes

      -- dataOffset is based on unescaped length.
      -- Points to the initialised memory location at runtime.
      , dataOffset =
          (dataOffset state) + (fromIntegral $ BS.length bytes)
      }


  -- Escape bytes for writing data segments in Wasm text format
  bytesToBuilderEscaped :: ByteString -> B.Builder
  bytesToBuilderEscaped bytes =
    -- Files are always UTF-8 so it's safe to fold byte-by-byte
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


  -- Convert Int32 to ByteSring (little-endian and always length 4)
  int32toBytes :: Int32 -> ByteString
  int32toBytes i32 =
    let
      (bytes, _) =
        BS.unfoldrN 4
          (\(remainder, count) ->
            if count < 0 then
              Nothing
            else
              let
                thisByte =
                  fromIntegral remainder .&. 255

                nextAccumulator =
                  ( remainder `shiftR` 8
                  , count - 1
                  )
              in
                Just (thisByte, nextAccumulator)
          )
          (i32, 3)
    in
      bytes


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


  comparableCtor :: ComparableCtor -> ByteString
  comparableCtor ctor =
    BS.singleton $
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
  generate expression state =
    case expression of
      Opt.Bool bool ->
        addInstr
          (i32_const $ if bool then 1 else 0)
          state
  
      Opt.Chr char ->
        let
          memoryInit =
            comparableCtor CompChar
            <> encodeText char
        in
          addData memoryInit $
            addInstr (i32_const $ dataOffset state) $
            state
  
      Opt.Str string ->
        let
          numCodePoints =
            int32toBytes $
              fromIntegral $
              Text.length string

          memoryInit =
            comparableCtor CompString
            <> numCodePoints
            <> encodeText string
        in
          addData memoryInit $
            addInstr (i32_const $ dataOffset state) $
            state

      Opt.Int int ->
        let
          memoryInit =
            comparableCtor CompInt
            <> (int32toBytes $ fromIntegral int)
        in
          addData memoryInit $
            addInstr (i32_const $ dataOffset state) $
            state
  
      Opt.Float value ->
        let
          -- TODO: Work out how to convert Double to raw bytes, not decimal text
          --  (Binary.encode seems to give way more than 8 bytes)
          -- Workaround: Initialise memory to zeros, then overwrite real value 
          -- in 'start' function. Then I only have to output decimal text.
          memoryInit =
            comparableCtor CompFloat
            <> (BS.pack $ take 8 $ repeat 0)
          
          addressInstr =
            i32_const $ dataOffset state
          
          startInstr =
            f64_store 0 addressInstr (f64_const value)
        in
          addData memoryInit $
            addInstr addressInstr $
            addStartInstr startInstr $
            state
  
      Opt.VarLocal name ->
        generateVarLocal name state
  
      Opt.VarGlobal (Opt.Global home name) ->
        addInstr
          (get_global $ Identifier.fromGlobal home name)
          state
  
      Opt.VarEnum (Opt.Global home name) index ->
        addInstr
          (get_global $ Identifier.fromGlobal home name)
          state
  
      Opt.VarBox (Opt.Global home name) ->
        addInstr
          (get_global $ Identifier.fromGlobal ModuleName.basics N.identity)
          state
  
      Opt.VarCycle home name ->
        addInstr
          (call_indirect
            (Identifier.fromFuncType [] I32)
            (get_global $ Identifier.fromCycle home name)
            [])
          state
  
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
        state
        -- JsExpr $ generateCall mode func args
  
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
      (get_local $ Identifier.fromLocal name)
      (maybeInsertLocalClosedOver state name)
  

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
      -- Recurse into the body
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

      -- Unique table index & function label

      tableOffset =
        tableSize bodyState

      funcId =
        FunctionName ("$elmFunc" <> B.int32Dec tableOffset)

      
      -- Closure data structure
      --   Generate code to construct and destructure it (args & closed-over values)
      --   Work out which names are closed-over, and pass them down through nested scopes
      --   Create a temporary variable to help with construction & copying

      closureTempName =
        N.fromString $ "$$closure" ++ show tableOffset

      closureTempLocalId =
        Identifier.fromLocal closureTempName

      closedOverSet =
        closedOverNames $ currentScope bodyState

      surroundingScope =
        currentScope state
  
      updatedSurroundingScope =
        addClosureToSurroundingScope closureTempName closedOverSet surroundingScope

      -- Generated function has just one param, the closure value
      funcArgId =
        LocalIdx 0

      (closureConstructCode, closureDestructCode) =
        generateClosure args closedOverSet closureTempLocalId funcArgId tableOffset
    
      closureConstructBlock =
        block 
          (LabelName $ "$createClosure" <> B.int32Dec tableOffset)
          I32
          (closureConstructCode ++ [get_local closureTempLocalId])

      -- AST for the Wasm function
      func =
        Function
          { _functionId = funcId
          , _params = [(funcArgId, I32)]
          , _locals = map (\name -> (Identifier.fromLocal name, I32)) $
                       Set.toList closedOverSet
          , _resultType = Just I32
          , _body = closureDestructCode ++ (reverse $ revInstr bodyState)
          }
    in
      bodyState
        { revInstr = closureConstructBlock : (revInstr state)
        , currentScope = updatedSurroundingScope
        , revFunc = func : (revFunc bodyState)
        , tableSize = 1 + tableSize bodyState
        , revTableFuncIds = funcId : revTableFuncIds bodyState
        }


  addClosureToSurroundingScope :: N.Name -> Set.Set N.Name -> Scope -> Scope
  addClosureToSurroundingScope closureTempName closedOverSet surroundingScope =
    let
      allSurroundingNames =
        Set.unions
          [ argNames surroundingScope
          , localNames surroundingScope
          , closedOverNames surroundingScope
          ]
      
      -- If any closed-over names skip this scope (defined in a higher scope),
      -- then they are implicitly closed-over in this scope too.
      skipScopeNames =
        Set.filter
          (\name -> not $ Set.member name allSurroundingNames)
          closedOverSet
    in
      surroundingScope
        { localNames =
            Set.insert
              closureTempName
              (localNames surroundingScope)
        , closedOverNames =
            Set.union (closedOverNames surroundingScope) skipScopeNames
        }


  generateClosure :: [N.Name] -> Set.Set N.Name -> LocalId -> LocalId -> Int32 -> ([Instr], [Instr])
  generateClosure args closedOverSet scopeId funcArgId elemIdx =
    let
      nArgs = length args

      i32size = 4
      pointersSize = (nArgs + Set.size closedOverSet) * i32size
      elemIndexSize = i32size
      aritySize = i32size
      totalSize = elemIndexSize + aritySize + pointersSize

      createNewClosure =
        set_local scopeId
          (call
            (_functionId gcAllocate)
            [i32_const $ fromIntegral totalSize]
          )

      storeElemIndex =
        i32_store 0 (get_local scopeId) $
          (i32_const elemIdx)

      storeArity =
        i32_store 4 (get_local scopeId) $
          (i32_const $ fromIntegral $ length args)

      (storeClosedOvers, destructClosedOvers) =
        generateClosedOverValues nArgs closedOverSet scopeId funcArgId

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
  generateClosedOverValues nArgs closedOverSet scopeId funcArgId =
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
                generateClosureInsert scopeId name byteOffset

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
  generateClosureInsert scopeId name byteOffset =
    i32_store byteOffset
      (get_local scopeId)
      (get_local (Identifier.fromLocal name))


  generateClosureDestruct :: LocalId -> N.Name -> Int -> Instr
  generateClosureDestruct funcArgId name byteOffset =
    set_local (Identifier.fromLocal name) $
      i32_load byteOffset (get_local funcArgId)
              

  gcAllocate :: Function
  gcAllocate =
    Function
      { _functionId = FunctionName "$gcAllocate"
      , _params = [(LocalName "$size", I32)]
      , _locals = []
      , _resultType = Just I32
      , _body = [unreachable] -- TODO
      }