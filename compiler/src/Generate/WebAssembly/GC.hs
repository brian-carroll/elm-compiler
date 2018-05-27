module Generate.WebAssembly.GC where

  import Generate.WebAssembly.DSL
  import Generate.WebAssembly.Ast

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


  deepCopy =
    let
      old = LocalName "$old"
      new = LocalName "$new"
      copy = FuncName "$copy"

      copyAtOffset n =
        i32_store n
          (get_local new)
          (call copy
            [i32_load n
              (get_local new)
            ]
          )
    in
      Func
        { name = "$deepCopy"
        , params = [(old, I32)]
        , locals = [(new, I32)]
        , returnType = Just I32
        , body =
          [ set_local new $
              call copy $
                [get_local old]
          , br_table
              (i32_load8_u 0 (get_local new))
              [(LabelName "$int",
                  [ get_local new
                  , return_
                  ])
              , (LabelName "$list"
                , map copyAtOffset [1, 2]
                )
              ]
              ( LabelName "$default"
              , [unreachable]
              )
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
