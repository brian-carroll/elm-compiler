{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.WebAssembly.Kernel (generate) where

  import Data.Int (Int32)
  import qualified Data.List as List
  import Data.Map (Map)
  import qualified Data.Map as Map

  import qualified AST.Module.Name as ModuleName

  import qualified Generate.WebAssembly.Kernel.Basics as Basics
  import Generate.WebAssembly.Kernel.State


  kernelModules :: Map ModuleName.Canonical [KernelState -> KernelState]
  kernelModules =
    Map.fromList
      [ (ModuleName.basics, Basics.exports)
      ]


  generate :: ModuleName.Canonical -> Int32 -> Int32 -> KernelState
  generate moduleName tableOffset dataOffset =
    case Map.lookup moduleName kernelModules of
      Nothing ->
        undefined

      Just moduleExports ->
        List.foldl'
          (\kState exportGen -> exportGen kState)
          (KernelState [] tableOffset dataOffset)
          moduleExports
