module Generate.WebAssembly.GC (..) where
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