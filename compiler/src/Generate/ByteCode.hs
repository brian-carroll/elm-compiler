{-# LANGUAGE OverloadedStrings #-}
module Generate.ByteCode
  ( generate
  )
  where

{-
  A common Intermediate Representation for targets like WebAssembly, LLVM, or C.
  It knows all the byte-level details of the program.

  Short term:    slowly refactor parts of the C generator to use ByteCode
  Medium term:   generate all C via ByteCode
  Long term:     create LLVM backend
-}

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Name as N

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Generate.ByteCode.Name (Name)
import qualified Generate.JavaScript as JS

data BcGenState =
  BcGenState
    { stateLiterals :: BcGenLiterals
    , stateGlobals :: Map.Map Opt.Global Global -- cache
    , stateProgram :: Program
    , stateJs :: JS.State
    }

data BcGenLiterals =
  BcGenLiterals
    { litInt :: Set.Set Int
    , litFloat :: Set.Set EF.Float
    , litChr :: Set.Set ES.String
    , litStr :: Set.Set ES.String
    , litAccessor :: Set.Set N.Name
    , litFieldGroup :: Set.Set [N.Name]
    , litField :: Set.Set N.Name
    , litCtor :: Set.Set N.Name
    , litJsKernel :: Set.Set N.Name N.Name
    , litPorts :: Set.Set Opt.Global -- JS values with global name format
    }


data Program =
  Program
    { progLiterals :: [(Name, ConstValue)]
    , progGlobals :: [Global]    -- dependency order
    , progRoots :: [Name]        -- for init
    , progKernels :: [Name]      -- for linking to JS
    , progMains :: [Name]        -- for linking to JS
    }

data Global
  = GlobalConst Name ConstValue
  | GlobalRoot Procedure
  | GlobalKernel Name Name

data ConstValue
  = ConstStruct TypeTag [StructBody] [ConstValue]
  | ConstClosure Int [ConstValue] -- arity, pre-applied args
  | ConstData [StructBody]
  | ConstRef Name

data StructBody
  = StructInt32 Int
  | StructInt16 Int
  | StructFloat EF.Float

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
    , evalArgs :: [Name]
    , evalLocals :: [Name]
    , evalInstructions :: [Instruction]
    }

data Instruction
  = LoadConst Name
  | LoadEvalFunc Name
  | LoadLocal Name
  | LoadGcRoot Name
  | LoadBody StructInfo Int Instruction
  | LoadChild StructInfo Int Instruction
  | StoreLocal Name Instruction
  | StoreGcRoot Name Instruction
  | StoreBody StructInfo Int Instruction Instruction
  | StoreChild StructInfo Int Instruction Instruction
  | Copy Instruction
  | Construct TypeTag BS.ByteString [Instruction]
  | CallProc Instruction [Instruction]
  | Block LabelId Instruction
  | ExitBlock LabelId
  | TestTypeTag TypeTag Instruction
  | FindFieldPos FieldId Instruction
  | NumOpBinary NumType NumOpBinary Instruction Instruction
  | NumOpUnary NumType NumOpUnary Instruction
  | NumOpConvert NumType NumType Instruction


newtype LabelId = LabelId Int


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
