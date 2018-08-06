{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.WebAssembly.Kernel.Basics (exports) where

  import Data.Int (Int32)
  import qualified Data.Binary.Put as Put
  import qualified Data.ByteString.Lazy as BSL

  import qualified Elm.Name as N

  import Generate.WebAssembly.AST
  import Generate.WebAssembly.Instructions
  import qualified Generate.WebAssembly.Identifier as Id
  import Generate.WebAssembly.Kernel.State
  import qualified Generate.WebAssembly.Kernel.GC as GC


  exports :: [KernelState -> KernelState]
  exports =
      [ add
      ]


  add :: KernelState -> KernelState
  add (KernelState decls tableOffset dataOffset) =
    let
      gid@(GlobalName nameBuilder) =
        Id.fromKernel N.basics (N.fromString "add")
      globalDecl =
        Global gid Immutable I32 (i32_const dataOffset)
      fid =
        FunctionName nameBuilder

      closure = LocalName "$closure"
      a = LocalName "$a"
      b = LocalName "$b"
      result = LocalName "$result"

      -- Closure memory layout
      sizeOffset = 0
      firstPtrOffset = sizeOffset + 4
      elemIdxOffset = firstPtrOffset + 4
      arityOffset = elemIdxOffset + 4
      closureArg0Offset = arityOffset + 4
      closureArg1Offset = closureArg0Offset + 4

      -- Number memory layout
      ctorOffset = firstPtrOffset + 4
      valueOffset = ctorOffset + 4
      floatCtor = 5

      getLocals :: [Instr]
      getLocals =
        [ comment "getLocals"
        , set_local a $
            i32_load closureArg0Offset $
              get_local closure
        , set_local result $
            (call
              (_functionId GC.shallowCopy)
              [ tee_local b $
                  i32_load closureArg1Offset $
                    get_local closure
              ]
            )
        ]

      isFloat :: Instr
      isFloat =
        commented "isFloat" $
        i32_eq
          (i32_const floatCtor)
          (i32_load ctorOffset $
            get_local a)

      addFloat :: Instr
      addFloat =
        commented "addFloat" $
        f64_store valueOffset
          (get_local result)
          (f64_add
            (f64_load valueOffset $
              get_local a)
            (f64_load valueOffset $
              get_local b))
      
      addInt :: Instr
      addInt =
        commented "addInt" $
        i32_store valueOffset
          (get_local result)
          (i32_add
            (i32_load valueOffset $
              get_local a)
            (i32_load valueOffset $
              get_local b))

      body :: [Instr]
      body =
        getLocals ++
        [ IfElse
            { _label = Nothing
            , _retType = Nothing
            , _if = isFloat
            , _then = [addFloat]
            , _else = [addInt]
            }
        , get_local result
        ]

      func =
        Function
          { _functionId = fid
          , _params = [(closure, I32)]
          , _locals =
              [ (a, I32)
              , (b, I32)
              , (result, I32)
              ]
          , _resultType = Just I32
          , _body = body
          }

      element =
        ElementSegment tableOffset [fid]
      
      gcSize = 20
      gcFirstPtr = 8

      dataSegment =
        DataSegment dataOffset $ mconcat $
          map (BSL.toStrict . Put.runPut . Put.putInt32le)
            [gcSize, gcFirstPtr, tableOffset, 2, 0, 0]
    in
      KernelState
        { _declarations =
            globalDecl : func : element : dataSegment : decls
        , _tableSize =
            tableOffset + 1
        , _dataOffset =
            dataOffset + 4 + gcSize
        }


  {-
  
  need:
    a Wasm function
    a table element  
    a global pointing to the closure
    a closure object in DataSegment
      gcHeader, elemIdxOffset, arity, arg1, arg2

  also GC stuff!!
    GC.shallowCopy
      get total bytesize
      allocate
      loop
        copy byte

    gcAllocate
      get global top-of-heap
      store in a local
      increment global top-of-heap by bytesize arg
      grow memory if needed
      return local var
    
    GC header
      only really need size for now, not necessarily first pointer
      but might as well put it in there
      To LEB or not to LEB? not to LEB

    might need expression generator stuff!!
    circular imports?
    no, Kernel only gets called from WebAssembly.hs
    make a ready-made ExprState
    accept existing state
    in WebAssembly.hs, flush it

  -}

  {-
    Most of 'add' is boilerplate for constructing and destructuring closures, applying args, etc.
    
    Can we create Kernel.Utils.apply and call to deal with this?
    Or some A2, F2 style functions?
    Can I take a two-arg Wasm function and wrap it using an F2 equivalent?
    Then the generated code would be smaller and more readable

    Closed-over values would become params of the Wasm function

    Code gen:
      create a Wasm function with one input per arg & closed-over
      constructor for this particular Elm function:
          gcSize | gcFirstPtr | tableIdx | originalArity | remainingArity | ptr1 | ptr2 | ...
    
    Kernel:

      apply(closure, a)
        if remainingArity==1 then
          get original arity
          get the remaining args out of the closure till the number matches original arity
          get everything lined up on the stack
          jump table on original arity
          call_indirect
        else
          insert args into closure
          return it

      multiple apply functions or one?
      if many
        end up with lots of those jump tables
        but can save inserting and removing from closure
  
  -}