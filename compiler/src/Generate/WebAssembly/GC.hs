module Generate.WebAssembly.GC where

  import Generate.WebAssembly.Instructions
  import Generate.WebAssembly.AST
    ( Function(..)
    , Instr
    , LabelId(..)
    , LocalId(..)
    , FunctionId(..)
    , ValType(..)
    )

  {-
  
  Allocate
    Arg: Expr type
    Retrieve top-of-heap pointer
    Grow the heap
      Map Expr type to number of bytes
      Not recursive, keep the pointers
    Store new top-of-heap
    Return old top-of heap

  Copy
    Arg: Expr type
    Allocate, get new target address
    Copy
      Number of bytes depends on Expr type
      Just copy one level, don’t follow pointers
        This makes persistent data structures work

  Deep copy
    Mainly for GC
    Recursive calls to Copy

  GC
    Retrieve current top-of-heap
    Retrieve “main” pointer
    Recursively copy all descendants to top of heap
      i.e. move them out of the “nursery”, as Haskell calls it
      Immutability => “main” should be small relative to garbage
    Update “main” pointer
    Subtract old from new top-of-heap => size of “main”
    If (enough room under new “main”) AND (new top-of-heap is “too high”)
      Copy “main” back down to bottom of heap to save memory
        Just above the global values
      Shrink Wasm “memory”
      Update “main” pointer
  -}






  loadElmType :: Instr -> Instr
  loadElmType pointer =
    i32_load8_u 0 pointer

    {-
    
    Need special functions for variable-length thingies
    Anything that has a list in it
      String    [Word32]
      Call      [NameDeref]
      TailCall  [NameDeref]
      If        [(Pointer, Pointer)]
      Destruct  [Word32]
      Update    [(RecordFieldIdx, Pointer)]
      Record    [(RecordFieldIdx, Pointer)]
      Env       [Pointer]

    -}

  deepCopyEnv :: Function
  deepCopyEnv =
    let
      old = LocalName "$old"
      new = LocalName "$new"
    in
      Function
        { functionId = FunctionName "$gcDeepCopyEnv"
        , params = [(old, I32)]
        , locals = [(new, I32)]
        , returnType = Just I32
        , body = [] -- TODO
        }


  deepCopyFields :: Function
  deepCopyFields =
    let
      old = LocalName "$old"
      new = LocalName "$new"
    in
      Function
        { functionId = FunctionName "$gcDeepCopyFields"
        , params = [(old, I32)]
        , locals = [(new, I32)]
        , returnType = Just I32
        , body = [] -- TODO
        }
      

  deepCopy :: Function
  deepCopy =
    let
      old = LocalName "$old"
      new = LocalName "$new"
      fid = FunctionName "$gcDeepCopy"

      deepCopyChildren :: [Int] -> [Instr]
      deepCopyChildren childOffsets =
        (map
          (\offset ->
            i32_store offset
              (get_local new)
              (call fid
                [i32_load offset (get_local new)]
              )
          )
          childOffsets
        )
        ++ [ get_local new
           , return_
           ]
    in
      Function
        { functionId = fid
        , params = [(old, I32)]
        , locals = [(new, I32)]
        , returnType = Just I32
        , body =
          [ set_local new $
              call (FunctionName "$gcShallowCopy") $ -- TODO
                [get_local old]
          , br_table
              (loadElmType (get_local new))
              (map (\(str, instr) -> ( LabelName ("$gcDeepCopy" ++ str) , instr ))
                [ ( "Bool", deepCopyChildren [] )
                , ( "Chr", deepCopyChildren [] )
                , ( "Str", deepCopyChildren [] )
                , ( "Int", deepCopyChildren [] )
                , ( "Float", deepCopyChildren [] )
                , ( "VarLocal", deepCopyChildren [] )
                , ( "VarGlobal", deepCopyChildren [1] )
                , ( "VarEnum", deepCopyChildren [] )
                , ( "VarBox", deepCopyChildren [1] )
                , ( "VarCycle", deepCopyChildren [1] )
                , ( "VarDebug", deepCopyChildren [] )
                , ( "VarKernel", deepCopyChildren [1] )
                , ( "List", deepCopyChildren [1, 2] )
                , ( "Function", (call (functionId deepCopyEnv) [get_local new])
                                  : deepCopyChildren [7]
                  )
                , ( "Call", deepCopyChildren [] )
                , ( "TailCall", deepCopyChildren [] )
                , ( "If", deepCopyChildren [] )
                , ( "Let", deepCopyChildren [] )
                , ( "Destruct", deepCopyChildren [] )
                , ( "Accessor", deepCopyChildren [] )
                , ( "Access", deepCopyChildren [] )
                , ( "Update", deepCopyChildren [] )
                , ( "Record", deepCopyChildren [] )
                , ( "Unit", deepCopyChildren [] )
                , ( "Tuple", deepCopyChildren [] )
                , ( "Shader", deepCopyChildren [] )
                ]
              )
              ( LabelName "$default", [unreachable] )
          ]
        }


    {-

        (i32.store, offset=2
          (get_local $new)
          (call $copy
            (i32.load offset=2
              (get_local $new)
            )
          )
        )

        (i32.store, offset=2
          (get_local $new)
          (call $copy
            (i32.load offset=2
              (get_local $new)
            )
          )
        )

    -}
-- get_local $old
-- call $copy
-- tee_local $new
-- get_local $old
-- load offset 0
-- br_table
-- $int
-- $list

-- label $int
-- return

-- label $list
-- get_local $new
-- get_local $new
-- load offset 1
-- call $copy
-- store, offset 1
-- get_local $new
-- get_local $new
-- load, offset 2
-- call $copy
-- store, offset 2
-- return
