module Generate.WebAssembly.Kernel.State where

  import Data.Int (Int32)
  import Generate.WebAssembly.AST (Declaration, Instr)

  data KernelState =
    KernelState
      { _declarations :: [Declaration]
      , _tableSize :: Int32
      , _dataOffset :: Int32
      }
