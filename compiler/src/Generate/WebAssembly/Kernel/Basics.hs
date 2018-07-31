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

      closureValue = LocalIdx 0
      returnValue = LocalName "$return"

      elemIdx = 0
      size = elemIdx + 4
      first_pointer = size + 4
      arity = first_pointer + 4
      ctor0 = arity + 4
      argPtr0 = ctor0 + 4
      argPtr1 = argPtr0 + 4
      compCtor = 4

      floatCtor = 5

      isFloat :: Instr
      isFloat =
        (i32_eq
          (i32_const floatCtor)
          (i32_load ctor0 $
            get_local closureValue))

      addFloat :: Instr
      addFloat =
        f64_store compCtor
          (tee_local returnValue
            (call
              (_functionId GC.shallowCopy)
              [i32_load argPtr0 $ get_local closureValue]))
          (f64_add
            (f64_load compCtor
              (i32_load argPtr0 $
                get_local closureValue))
            (f64_load compCtor
              (i32_load argPtr1 $
                get_local closureValue)))

      
      addInt :: Instr
      addInt =
        i32_store compCtor
          (tee_local returnValue
            (call
              (_functionId GC.shallowCopy)
              [i32_load argPtr0 $ get_local closureValue]))
          (i32_add
            (i32_load compCtor
              (i32_load argPtr0 $
                get_local closureValue))
            (i32_load compCtor
              (i32_load argPtr1 $
                get_local closureValue)))

      body :: [Instr]
      body =
        [ IfElse
            { _label = Nothing
            , _retType = Nothing
            , _if = isFloat
            , _then = [addFloat]
            , _else = [addInt]
            }
        , get_local returnValue
        ]

      func =
        Function
          { _functionId = fid
          , _params = [(closureValue, I32), (returnValue, I32)]
          , _locals = []
          , _resultType = Just I32
          , _body = body
          }

      element =
        ElementSegment tableOffset [fid]

      dataSegment =
        DataSegment dataOffset $ mconcat $
          map (BSL.toStrict . Put.runPut . Put.putInt32le)
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


  {-
  
  need:
    a Wasm function
    a table element  
    a global pointing to the closure
    a closure object in DataSegment
      gcHeader, elemIdx, arity, arg1, arg2

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