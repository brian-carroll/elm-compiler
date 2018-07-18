{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.AST where

import Data.Int (Int32)
import Data.Map (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))

data TypeId = TypeIdx Int | TypeName Builder
data FunctionId = FunctionIdx Int | FunctionName Builder
data TableId = TableIdxZero -- index must be 0 in Wasm MVP
data MemId = MemIdxZero -- index must be 0 in Wasm MVP
data GlobalId = GlobalIdx Int | GlobalName Builder
data LocalId = LocalIdx Int | LocalName Builder
data LabelId = LabelIdx Int | LabelName Builder

data Limits = Limits Int32 (Maybe Int32)
data Mutability = Mutable | Immutable

data ValType = I32 | I64 | F32 | F64
data ElemType = AnyFunc


data Module =
  Module
    { _imports :: [Import]
    , _mems :: Maybe Memory
    , _tables :: Maybe Table
    , _start :: Maybe FunctionId
    , _decls :: [Declaration]
    }


-- General declarations. Can have any number in any order.
data Declaration
  = FuncType (Maybe TypeId) [ValType] (Maybe ValType)
  | Global GlobalId Mutability ValType Instr
  | ElementSegment Int32 [FunctionId]
  | DataSegment Int32 Builder
  | Export Builder ImportExportDesc
  | Function
      { _functionId :: FunctionId
      , _params :: [(LocalId, ValType)]
      , _locals :: [(LocalId, ValType)]
      , _resultType :: Maybe ValType
      , _body :: Expr
      }


data Memory =
  Memory MemId Limits


data Table
  = TableDeclaration Limits ElemType
  | TableInlineDef ElemType [FunctionId]


data Import =
  Import Builder Builder ImportExportDesc


data ImportExportDesc
  = ImpExpFunc FunctionId
  | ImpExpTable TableId
  | ImpExpMem MemId
  | ImpExpGlobal GlobalId


type Expr = [Instr]


data Instr
  = Unreachable
  | Nop
  | Block LabelId ValType [Instr]
  | Loop LabelId ValType [Instr]
  | IfElse
      { _label :: Maybe LabelId
      , _retType :: ValType
      , _if :: Instr
      , _then :: [Instr]
      , _else :: [Instr]
      }
  | Br LabelId
  | BrIf LabelId Instr
  | BrTable [LabelId] LabelId Instr
  | Return
  | Call FunctionId [Instr]
  | CallIndirect TypeId Instr [Instr]
  | GetLocal LocalId
  | SetLocal LocalId Instr
  | TeeLocal LocalId Instr
  | GetGlobal GlobalId
  | SetGlobal GlobalId Instr
  | Drop Instr
  | Select
      { _select0 :: Instr
      , _select1 :: Instr
      , _selectCond :: Instr
      }
  | ConstOp
      { _literal :: Builder
      , _constType :: ValType
      }
  | Op
      { _opCode :: Builder
      , _operands :: [Instr]
      }
  | MemOp
      { _opCode :: Builder
      , _memarg :: MemArg
      , _operands :: [Instr]
      }
  | Comment Builder
  | Commented Builder Instr
  


data MemArg = MemArg { _memOffset :: Int, _align :: MemAlign }


data MemAlign
  = Align8
  | Align16
  | Align32
  | Align64
  | AlignNatural


  
-- Helper functions for Instructions DSL

load :: Builder -> Int -> Instr -> Instr
load opCode offset operand =
  MemOp
    { _opCode = opCode
    , _memarg = MemArg offset AlignNatural
    , _operands = [operand]
    }

store :: Builder -> Int -> Instr -> Instr -> Instr
store opCode offset address value =
  MemOp
    { _opCode = opCode
    , _memarg = MemArg offset AlignNatural
    , _operands = [address, value]
    }

unop :: Builder -> Instr -> Instr
unop opCode operand =
  Op
    { _opCode = opCode
    , _operands = [operand]
    }

binop :: Builder -> Instr -> Instr -> Instr
binop opCode operand1 operand2 =
  Op
    { _opCode = opCode
    , _operands = [operand1, operand2]
    }
