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

  import qualified AST.Optimized as Opt
  import Generate.WebAssembly.AST
  import Generate.WebAssembly.Instructions
  import Generate.WebAssembly.Builder as WA
  import qualified Elm.Name as N
  import qualified Generate.WebAssembly.Label as Label


  -- EXPRESSION GENERATOR STATE  
  
  data ExprState =
    ExprState
      { revInstr :: [Instr]
      , revFunc :: [Function]
      , dataSegment :: B.Builder
      , dataOffset :: Int32
      , revTableFuncIds :: [FunctionId]
      , revStartInstr :: [Instr]
      }


  addInstr :: Instr -> ExprState -> ExprState
  addInstr instr state =
    state { revInstr = instr : (revInstr state) }


  addStartInstr :: Instr -> ExprState -> ExprState
  addStartInstr instr state =
    state { revStartInstr = instr : (revStartInstr state) }
  

  addFunc :: Function -> ExprState -> ExprState
  addFunc func state =
    state { revFunc = func : (revFunc state) }


  addFuncIdToTable :: FunctionId -> ExprState -> ExprState
  addFuncIdToTable fid state =
    state { revTableFuncIds = fid : (revTableFuncIds state) }


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


  -- Escape bytes for writing data segments to a Wasm text format file
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


  -- addDataInt32 :: Int32 -> ExprState -> ExprState
  -- addDataInt32 i state =
  --   let
  --     x = 0
  --   in
  --     state
  

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
  
      {-
          need an envDeps Dict in ExprState
          add them as we encounter them
          when we pop back up to outer function that creates this one,
          generate code to populate it

          what about 'let' names? they're people too.
          Easiest thing is to make them part of the Env
          Could make them special cases using Wasm locals but that means
          distinguishing them correctly when current compiler doesn't
          Downside is extra memory
          Unless we implement let-in as a lambda, creating another function
          inside the function. Inner function treats 'lets' as input params
          and real inputs as closed-over values.
          But is there a good way to detect this?

          Lets are recursive in Elm AST
          They come in dependency order
          So on every Let Def, add to the locals Map
          prepopulate the Map with args in reverse order
            before recursing into the body
            guarantees they will be in the right place in the map
          for each Let Def in the body, insert an entry to the Map
          for each VarLocal that isn't already in the Map, must be 
            a closed-over value.
          Create an ADT,
              Local = Arg Int | ClosedOver Int | Let
          locals Map has this ADT as values


        Make all args & VarLocals & closed-over values into Wasm locals

          They would become easier to fetch (registers?)
          On entry into function, set all the locals from Env
          Should be quick. Two instructions per param
            (load with fixed offset, then set_local)
          We'll have to load them at least once anyway, so that's free
            and things have names => generated code is nice and readable
            and VarLocal is more intuitive, similar to JS meaning
            and we're guaranteed to only load the pointer once
            and we don't have to pass around empty Env slots


        Function code gen:
          Take the list of params and create a Locals Map from it,
            with ADT specifying 'Arg'
          Push that onto the localMaps List in the ExprState

          Recurse into the body, generate it and get the returned ExprState

          Generate the Env destructuring code
            put the Args and ClosedOvers in the Env
          
          Prepend Env destructuring code to the main body

          Return the ExprState
            drop the head of the localMaps List
            get next Table Element index and increment it
            write Table Element declaration
            write DataSegment declaration for empty Env (Arg & ClosedOver)
            write the Wasm Function declaration
            add an Instruction i32.const (dataOffset before adding Env)
        
        Non-tail recursion
          Need own function name as a closed-over variable
          Does this come for free?
            yes if we insert child function into parent env
              *before* building child closure value
      -}
      Opt.VarLocal name ->
        addInstr
          (get_local $ Label.fromLocal name)
          state
  
      Opt.VarGlobal (Opt.Global home name) ->
        addInstr
          (get_global $ Label.fromGlobal home name)
          state
  
      Opt.VarEnum (Opt.Global home name) index ->
        state
        -- case mode of
        --   Mode.Dev _ _ ->
        --     JsExpr $ JS.Ref (Name.fromGlobal home name)
  
        --   Mode.Prod _ _ ->
        --     JsExpr $ JS.Int (Index.toMachine index)
  
      Opt.VarBox (Opt.Global home name) ->
        state
        -- JsExpr $ JS.Ref $
        --   case mode of
        --     Mode.Dev _ _ -> Name.fromGlobal home name
        --     Mode.Prod _ _ -> Name.fromGlobal ModuleName.basics N.identity
  
      Opt.VarCycle home name ->
        addInstr
          (call_indirect
            (Label.fromFuncType [] I32)
            (get_global $ Label.fromCycle home name)
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
        state
        -- generateFunction (map Name.fromLocal args) (generate mode body)
  
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
  
  