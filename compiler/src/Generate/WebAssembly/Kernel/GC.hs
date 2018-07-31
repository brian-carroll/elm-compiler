{-# LANGUAGE OverloadedStrings #-}

module Generate.WebAssembly.Kernel.GC
  ( moduleName
  , global
  , exports
  , allocate
  , sizeof
  , shallowCopy
  )
  where

  import qualified AST.Module.Name as ModuleName
  import qualified Elm.Package as Pkg
  import qualified AST.Optimized as Opt
  import qualified Elm.Name as N

  import Generate.WebAssembly.Instructions
  import Generate.WebAssembly.AST
  import Generate.WebAssembly.Kernel.State


  -- ELM AST

  moduleName :: ModuleName.Canonical
  moduleName =
    ModuleName.Canonical Pkg.core "GC"


  global :: Opt.Global
  global =
      Opt.Global (ModuleName.Canonical Pkg.core "GC") (N.fromText "")


  exports :: [KernelState -> KernelState]
  exports =
      [ gc ]


  gc :: KernelState -> KernelState
  gc state =
    state
      { _declarations =
          [ heapTop
          , allocate
          , sizeof
          , shallowCopy
          ]
      }


  -- CONTENTS

  heapTopId :: GlobalId
  heapTopId =
    GlobalName "$_GC_heapTop"


  heapTop :: Declaration
  heapTop =
    Global heapTopId Mutable I32 (i32_const 0)


  allocate :: Declaration
  allocate =
    Function
      { _functionId = FunctionName "$_GC_allocate"
      , _params = [(LocalName "$size", I32)]
      , _locals = [(LocalName "$oldHeap", I32)]
      , _resultType = Just I32
      , _body =
          [ set_global heapTopId $
              i32_add
                (i32_const 1)
                (tee_local
                  (LocalName "$oldHeap")
                  (get_global heapTopId))
          , get_local
              (LocalName "$oldHeap")
          ]
      }


  sizeof :: Declaration
  sizeof =
    Function
      { _functionId = FunctionName "$_GC_sizeof"
      , _params = [(LocalIdx 0, I32)]
      , _locals = []
      , _resultType = Just I32
      , _body = [ i32_load 0 $ get_local $ LocalIdx 0 ]
      }


  shallowCopy :: Declaration
  shallowCopy =
    let
      old = LocalName "$old"
      new = LocalName "$new"
      size = LocalName "$size"
      returnPtr = LocalName "$returnPtr"
    in
      Function
        { _functionId = FunctionName "$_GC_shallowCopy"
        , _params = [(old, I32)]
        , _locals = [(new, I32), (size, I32), (returnPtr, I32)]
        , _resultType = Just I32
        , _body =
            [ set_local new $
                tee_local returnPtr $
                call (_functionId allocate)
                  [ tee_local size $
                      call (_functionId sizeof) [get_local old]
                  ]
            , loop (LabelName "$loop") Nothing
                [ i32_store 0 (get_local new) $
                    i32_load 0 (get_local old)
                , set_local old
                    (i32_add (get_local old) (i32_const 4))
                , set_local new
                    (i32_add (get_local new) (i32_const 4))
                , br_if (LabelName "$loop") $
                    i32_gt_s
                      (tee_local size
                        (i32_sub (get_local size) (i32_const 4)))
                      (i32_const 0)
                ]
            , get_local returnPtr
            ]
        }
