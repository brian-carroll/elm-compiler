module Generate.WebAssembly.Ast where


data TypeId = TypeIdx Int
data FuncId = FuncIdx Int | FuncName String
data TableId = TableIdx Int
data MemId = MemIdx -- numeric index must be 0, only one data segment allowed
data GlobalId = GlobalIdx Int | GlobalName String
data LocalId = LocalIdx Int | LocalName String
data LabelId = LabelIdx Int | LabelName String

data Limits = Limits Int (Maybe Int)
type MemType = Limits
data Mutability = Mutable | Immutable

data ValType = I32 | I64 | F32 | F64

data FuncType = FuncType [ValType] (Maybe ValType)
data TableType = TableType Limits ElemType
data ElemType = AnyFunc
data GlobalType = GlobalType Mutability ValType 

data Func =
  Func
  { name :: String
  , params :: [(LocalId, ValType)]
  , locals :: [(LocalId, ValType)]
  , returnType :: Maybe ValType
  , body :: Expr
  }

data DataSegment =
  DataSegment
  { memIdx :: MemId
  , dataOffset :: Expr
  , bytes :: [Int]
  }

data Import =
  Import String String ImportExportDesc
        
data Export =
  Export String ImportExportDesc

data ImportExportDesc
  = ImpExpFunc FuncId
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
  | Call FuncId [Instr]
  | CallIndirect FuncId [Instr]
  | GetLocal LocalId
  | SetLocal LocalId Instr
  | TeeLocal LocalId Instr
  | GetGlobal GlobalId
  | SetGlobal GlobalId Instr
  | Drop Instr
  | Select Instr Instr Instr
  | ConstOp
      { literal :: String
      , constType :: ValType
      }
  | Op
      { opCode :: String
      -- , operandTypes :: [ValType] -- not currently used
      -- , returnType :: Maybe ValType -- not currently used
      , subExprs :: [Instr]
      }
  | MemOp
      { opCode :: String
      -- , operandTypes :: [ValType] -- not currently used
      -- , returnType :: Maybe ValType -- not currently used
      , subExprs :: [Instr]
      , memarg :: MemArg
      }


data MemArg = MemArg { memOffset :: Int, align :: MemAlign }

data MemAlign
  = Align8
  | Align16
  | Align32
  | Align64
  | Natural

instance Show MemAlign where
  show Align8 = "3"
  show Align16 = "4"
  show Align32 = "5"
  show Align64 = "6"

data Module
  = Module
  { types :: [FuncType]
  , funcs :: [Func]
  , tables :: [TableType]
  , mems :: [MemType]
  , globals :: [GlobalType]
  , elem :: [ElemType]
  , data_ :: [DataSegment]
  , start :: Maybe FuncId
  , imports :: [Import]
  , exports :: [Export]
  }
    