{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.AST where

import Data.Map (Map)
import Data.ByteString.Builder (Builder)

-- TODO: Should the label names be Builder, Text, String, or what?
data TypeId = TypeIdx Int | TypeName Builder
data FunctionId = FunctionIdx Int | FunctionName Builder
data TableId = TableIdxZero -- index must be 0 in Wasm MVP
data MemId = MemIdxZero -- index must be 0 in Wasm MVP
data GlobalId = GlobalIdx Int | GlobalName Builder
data LocalId = LocalIdx Int | LocalName Builder
data LabelId = LabelIdx Int | LabelName Builder

data Limits = Limits Int (Maybe Int)
type MemType = Limits
data Mutability = Mutable | Immutable

data ValType = I32 | I64 | F32 | F64

data FuncType = FuncType (Maybe TypeId) [ValType] (Maybe ValType)
data TableType = TableType Limits ElemType
data ElemType = AnyFunc
data GlobalType = GlobalType Mutability ValType 


data Function =
  Function
    { functionId :: FunctionId
    , params :: [(LocalId, ValType)]
    , locals :: [(LocalId, ValType)]
    , returnType :: Maybe ValType
    , body :: Expr
    }


data DataSegment =
  DataSegment
    { memIdx :: MemId
    , dataOffset :: Instr
    , bytes :: Builder
    }


data Import =
  Import Builder Builder ImportExportDesc


data Export =
  Export Builder ImportExportDesc


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
      { ifElseType :: ValType
      , if_ :: Instr
      , then_ :: (Maybe LabelId, [Instr])
      , else_ :: (Maybe LabelId, [Instr])
      }
  | Br LabelId
  | BrIf LabelId Instr
  | BrTable Instr [(LabelId, [Instr])] (LabelId, [Instr])
  | Return
  | Call FunctionId [Instr]
  | CallIndirect FunctionId [Instr]
  | GetLocal LocalId
  | SetLocal LocalId Instr
  | TeeLocal LocalId Instr
  | GetGlobal GlobalId
  | SetGlobal GlobalId Instr
  | Drop Instr
  | Select Instr Instr Instr
  | ConstOp
      { literal :: Builder
      , constType :: ValType
      }
  | Op
      { opCode :: Builder
      , subExprs :: [Instr]
      }
  | MemOp
      { opCode :: Builder
      , subExprs :: [Instr]
      , memarg :: MemArg
      }


data MemArg = MemArg { memOffset :: Int, align :: MemAlign }


data MemAlign
  = Align8
  | Align16
  | Align32
  | Align64
  | AlignNatural


memAlignToBuilder :: MemAlign -> Builder
memAlignToBuilder align =
  case align of
    Align8 -> "3"
    Align16 -> "4"
    Align32 -> "5"
    Align64 -> "6"
    AlignNatural -> ""


data Global =
  Global
    { globalId :: GlobalId
    , globalType :: GlobalType
    , globalValue :: Instr
    }


-- TODO: some of the Module fields are missing values, have only types!
data Module =
  Module
    { types :: [FuncType]
    , funcs :: [Function]
    , tables :: [TableType]
    , mems :: [MemType]
    , globals :: [GlobalType]
    , elem :: [ElemType]
    , data_ :: [DataSegment]
    , start :: Maybe FunctionId
    , imports :: [Import]
    , exports :: [Export]
    }

  
-- Helper functions for Instructions DSL

load :: Builder -> Int -> Instr -> Instr
load opCode offset subExpr =
  MemOp
    { opCode = opCode
    , subExprs = [subExpr]
    , memarg = MemArg { memOffset = offset, align = AlignNatural }
    }

store :: Builder -> Int -> Instr -> Instr -> Instr
store opCode offset addrExpr valExpr =
  MemOp
    { opCode = opCode
    , subExprs = [addrExpr, valExpr]
    , memarg = MemArg { memOffset = offset, align = AlignNatural }
    }

unop :: Builder -> Instr -> Instr
unop opCode subExpr =
  Op
    { opCode = opCode
    , subExprs = [subExpr]
    }

binop :: Builder -> Instr -> Instr -> Instr
binop opCode subExpr1 subExpr2 =
  Op
    { opCode = opCode
    , subExprs = [subExpr1, subExpr2]
    }
