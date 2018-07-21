{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.WebAssembly.Kernel.Basics (exports) where

  import Data.Int (Int32)
  import qualified Data.Binary.Put as Put

  import qualified Elm.Name as N

  import Generate.WebAssembly.AST
  import Generate.WebAssembly.Instructions
  import Generate.WebAssembly.Identifier as Id
  import Generate.WebAssembly.Kernel.State


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

      closureValue = LocalIdx 0

      elemIdx = 0
      arity = elemIdx + 4
      ctor0 = arity + 4
      argPtr0 = ctor0 + 4
      argPtr1 = argPtr0 + 4
      compCtor = 4

      floatCtor = 5

      body :: Instr
      body =
        select
          (i32_store compCtor
            (call
              (_functionId gcShallowCopy)
              [i32_load argPtr0 $ get_local closureValue])
            (i32_add
              (i32_load compCtor
                (i32_load argPtr0 $
                  get_local closureValue))
              (i32_load compCtor
                (i32_load argPtr1 $
                  get_local closureValue))))
          (f64_store compCtor
            (call
              (_functionId gcShallowCopy)
              [i32_load argPtr0 $ get_local closureValue])
            (f64_add
              (f64_load compCtor
                (i32_load argPtr0 $
                  get_local closureValue))
              (f64_load compCtor
                (i32_load argPtr1 $
                  get_local closureValue))))
          (i32_eq
            (i32_const floatCtor)
            (i32_load ctor0 $
              get_local closureValue))

      func =
        Function
          { _functionId = fid
          , _params = [(closureValue, I32)]
          , _locals = []
          , _resultType = Just I32
          , _body = [ body ]
          }

      element =
        ElementSegment tableOffset [fid]


      {-
        WRONG!!!!
        NOT ESCAPED!!!
        Refactor DataSegment to take a ByteString,
        then escape it when converting to Builder
      -}
      dataSegment =
        DataSegment dataOffset $ mconcat $
          map (Put.execPut . Put.putInt32le)
            [tableOffset, 2, 0, 0]
    in
      KernelState
        { _declarations =
            globalDecl : func : element : dataSegment : decls
        , _tableSize =
            tableOffset + 1
        , _dataOffset =
            dataOffset + 4
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

  {-
  
  need:
    a Wasm function
    a table element  
    a global pointing to the closure
    a closure object in DataSegment
      gcHeader, elemIdx, arity, arg1, arg2

  also GC stuff!!
    gcShallowCopy
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