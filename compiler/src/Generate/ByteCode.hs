{-# LANGUAGE OverloadedStrings #-}
module Generate.C
  ( generate
  )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName


newtype ConstIndex    = ConstIndex    Int
newtype GlobalIndex   = GlobalIndex   Int
newtype LocalIndex    = LocalIndex    Int
newtype EvalFuncIndex = EvalFuncIndex Int
newtype LabelIndex    = LabelIndex    Int
newtype FieldIndex    = FieldIndex    Int
newtype StackCount    = StackCount    Int

data StructInfo =
  StructInfo
    { structNumDataSlots :: Int
    , structHasChildren :: Bool
    }

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

data ConstValue
  = ConstValue TypeTag BS.ByteString [HeapValue]

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


-- Should I split globals into const vs variable?
-- yep! (track which is which in the generator state)
data Instruction                                -- stack
  = LoadConst ConstIndex                        -- +1
  | LoadEvalFunc EvalFuncIndex                  -- +1 (for constructing closures)
  | LoadLocal LocalIndex                        -- +1
  | LoadGlobal GlobalIndex                      -- +1
  | LoadChild StructInfo Int                    -- -1 +1
  | LoadField FieldIndex                        -- -1 +1
  | StoreLocal LocalIndex                       -- -1
  | StoreGlobal GlobalIndex                     -- -1
  | StoreChild StructInfo Int                   -- -2
  | StoreField FieldIndex                       -- -2
  | Copy                                        -- -1 +1
  | Construct TypeTag BS.ByteString StackCount  -- -StackCount +1
  | Apply StackCount                            -- -StackCount +1
  | Label LabelIndex                            --  0
  | Jump LabelIndex                             --  0
  | JumpIfTrue LabelIndex                       -- -1
  | JumpIfFalse LabelIndex                      -- -1
  | TestTypeTag TypeTag                         -- -1 +1
  | NumOpBinary NumType NumOpBinary             -- -2 +1
  | NumOpUnary NumType NumOpUnary               -- -1 +1
  | NumOpConvert NumType NumType                -- -1 +1


data ByteCodeFunction =
  ByteCodeFunction
    { bcfName :: B.Builder
    , bcfArity :: Int
    , bcfTailCallOpt :: Bool
    , bcfInstructions :: [Instruction]
    }


data ByteCodeProgram =
  ByteCodeProgram
    { bcpConstants :: Map.Map GlobalIndex ConstValue
    , bcpEvalFunctions :: Map.Map EvalFuncIndex ByteCodeFunction
    , bcpNumGlobalVars :: Int
    }


-- GENERATE

type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main

generate :: Opt.GlobalGraph -> Mains -> (B.Builder, B.Builder)
generate (Opt.GlobalGraph graph fieldFreqMap) mains =
  ("", "")
