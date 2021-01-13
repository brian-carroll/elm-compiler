{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName


data Program =
  Program
    { progConstants :: Map.Map GcRootId ConstValue
    , progEvalFuncs :: Map.Map EvalFuncId Procedure
    , progNumGcRoots :: Int
    }

data ConstValue =
  ConstValue TypeTag BS.ByteString [HeapValue]

-- Do I really want int IDs? Even .obj and .wat files have symbolic names...
newtype ConstId    = ConstId    Int
newtype GcRootId   = GcRootId   Int
newtype LocalId    = LocalId    Int
newtype EvalFuncId = EvalFuncId Int
newtype LabelId    = LabelId    Int
newtype FieldId    = FieldId    Int

data TypeTag
  = ElmInt
  | ElmFloat
  | ElmChar
  | ElmString
  | Cons
  | Tuple2
  | Tuple3
  | Custom
  | Record
  | FieldGroup
  | Closure
  | JsRef

data Procedure =
  Procedure
    { evalName :: B.Builder
    , evalArgs :: Int
    , evalInstructions :: [Instruction]
    }

data Instruction
  = LoadConst ConstId
  | LoadEvalFunc EvalFuncId
  | LoadLocal LocalId
  | LoadGcRoot GcRootId
  | LoadChild StructInfo Int Instruction
  | LoadField FieldId Instruction
  | StoreLocal LocalId Instruction
  | StoreGcRoot GcRootId Instruction
  | StoreChild StructInfo Int Instruction Instruction
  | StoreField FieldId Instruction Instruction
  | Copy Instruction
  | Construct TypeTag BS.ByteString [Instruction]
  | Apply Instruction [Instruction]
  | Block LabelId Instruction
  | ExitBlock LabelId
  | TestTypeTag TypeTag Instruction
  | NumOpBinary NumType NumOpBinary Instruction Instruction
  | NumOpUnary NumType NumOpUnary Instruction
  | NumOpConvert NumType NumType Instruction


data StructInfo =
  StructInfo
    { structNumDataSlots :: Int
    , structHasChildren :: Bool
    }

data NumType = NumInt | NumFloat

data NumOpBinary
  = MulOp
  | DivOp
  | RmdOp   -- remainder of division
  | AddOp
  | SubOp
  | ShlOp   -- shift left
  | ShrOp   -- shift right
  | LtOp    -- less
  | GtOp    -- greater
  | LeOp    -- less or equal
  | GeOp    -- greater or equal
  | EqOp    -- equal
  | NeqOp   -- not equal
  | AndOp   -- bitwise and
  | XorOp   -- exclusive bitwise or
  | OrOp    -- inclusive bitwise or
  | LandOp  -- logical and
  | LorOp   -- logical or

data NumOpUnary
  = MinusOp
  | NotOp


-- GENERATE

type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main

generate :: Opt.GlobalGraph -> Mains -> (B.Builder, B.Builder)
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  ("", "")
