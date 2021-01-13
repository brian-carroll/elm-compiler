{-# LANGUAGE OverloadedStrings #-}
module Generate.ByteCode
  ( generate
  )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Generate.ByteCode.Name (Name)


data Program =
  Program
    { progConstants :: Map.Map Name ConstValue
    , progEvalFuncs :: Map.Map Name Procedure
    , progGcRootInits :: Map.Map Name Procedure
    , progMains :: [Name]
    }

data ConstValue
  = ConstStruct TypeTag BS.ByteString [ConstValue]
  | ConstUnboxed Int

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
  = LoadConst Name
  | LoadEvalFunc Name
  | LoadLocal Name
  | LoadGcRoot Name
  | LoadChild StructInfo Int Instruction
  | LoadField Name Instruction
  | StoreLocal Name Instruction
  | StoreGcRoot Name Instruction
  | StoreChild StructInfo Int Instruction Instruction
  | StoreField Name Instruction Instruction
  | Copy Instruction
  | Construct TypeTag BS.ByteString [Instruction]
  | Apply Instruction [Instruction]
  | Block LabelId Instruction
  | ExitBlock LabelId
  | TestTypeTag TypeTag Instruction
  | NumOpBinary NumType NumOpBinary Instruction Instruction
  | NumOpUnary NumType NumOpUnary Instruction
  | NumOpConvert NumType NumType Instruction


newtype LabelId =
  LabelId Int


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
