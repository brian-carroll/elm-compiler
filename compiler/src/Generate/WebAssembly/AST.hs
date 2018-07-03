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

data Limits = Limits Int (Maybe Int)
data Mutability = Mutable | Immutable

data ValType = I32 | I64 | F32 | F64
data FuncType = FuncType (Maybe TypeId) [ValType] (Maybe ValType)
data ElemType = AnyFunc
data GlobalType = GlobalType Mutability ValType 


data Module =
  Module
    { _types :: [FuncType]
    , _funcs :: [Function]
    , _globals :: [Global]
    , _tables :: Maybe Table
    , _elem :: [ElementSegment]
    , _mems :: Maybe Memory
    , _data :: [DataSegment]
    , _start :: Maybe StartFunction
    , _imports :: [Import]
    , _exports :: [Export]
    }


data Function =
  Function
    { _functionId :: FunctionId
    , _params :: [(LocalId, ValType)]
    , _locals :: [(LocalId, ValType)]
    , _resultType :: Maybe ValType
    , _body :: Expr
    }


data Global =
  Global
    { _globalId :: GlobalId
    , _globalType :: GlobalType
    , _globalValue :: Instr
    }
    

data Memory =
  Memory MemId Limits
    

data DataSegment =
  DataSegment
    { _memIdx :: MemId
    , _dataOffset :: Int32
    , _bytes :: ByteString
    }


data Table
  = TableDeclaration Limits ElemType
  | TableInlineDef ElemType [FunctionId]


data ElementSegment =
  ElementSegment
    { _tableOffset :: Int32
    , _functionIds :: [FunctionId]
    }
  
  
data StartFunction =
  Start FunctionId


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
  | BrTable [LabelId] LabelId Instr
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
      { _literal :: Builder
      , _constType :: ValType
      }
  | Op
      { _opCode :: Builder
      , _subExprs :: [Instr]
      }
  | MemOp
      { _opCode :: Builder
      , _memarg :: MemArg
      , _subExprs :: [Instr]
      }


data MemArg = MemArg { _memOffset :: Int, _align :: MemAlign }


data MemAlign
  = Align8
  | Align16
  | Align32
  | Align64
  | AlignNatural


  
-- Helper functions for Instructions DSL

load :: Builder -> Int -> Instr -> Instr
load opCode offset subExpr =
  MemOp
    { _opCode = opCode
    , _memarg = MemArg offset AlignNatural
    , _subExprs = [subExpr]
    }

store :: Builder -> Int -> Instr -> Instr -> Instr
store opCode offset addrExpr valExpr =
  MemOp
    { _opCode = opCode
    , _memarg = MemArg offset AlignNatural
    , _subExprs = [addrExpr, valExpr]
    }

unop :: Builder -> Instr -> Instr
unop opCode subExpr =
  Op
    { _opCode = opCode
    , _subExprs = [subExpr]
    }

binop :: Builder -> Instr -> Instr -> Instr
binop opCode subExpr1 subExpr2 =
  Op
    { _opCode = opCode
    , _subExprs = [subExpr1, subExpr2]
    }
