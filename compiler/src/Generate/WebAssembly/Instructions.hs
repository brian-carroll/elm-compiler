{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Instructions where

  {-
    Domain Specific Language for WebAssembly instructions

    Reference:
      https://webassembly.github.io/spec/core/appendix/index-instructions.html
  -}

import Data.Int (Int32, Int64)
import qualified Data.ByteString.Builder as B
import Generate.WebAssembly.AST
  ( Instr(..)
  , ValType(..)
  , load
  , store
  , unop
  , binop
  )


unreachable = Unreachable
nop = Nop
block = Block
loop = Loop
ifElse = IfElse
br = Br
br_if = BrIf
br_table = BrTable
return_ = Return

call = Call
call_indirect = CallIndirect

drop = Drop
select = Select

get_local = GetLocal
set_local = SetLocal
tee_local = TeeLocal
get_global = GetGlobal
set_global = GetGlobal

i32_load = load "i32.load"
i64_load = load "i64.load"
f32_load = load "f32.load"
f64_load = load "f64.load"
i32_load8_s = load "i32.load8_s"
i32_load8_u = load "i32.load8_u"
i32_load16_s = load "i32.load16_s"
i32_load16_u = load "i32.load16_u"
i64_load8_s = load "i64.load8_s"
i64_load8_u = load "i64.load8_u"
i64_load16_s = load "i64.load16_s"
i64_load16_u = load "i64.load16_u"
i64_load32_s = load "i64.load32_s"
i64_load32_u = load "i64.load32_u"

i32_store = store "i32.store"
i64_store = store "i64.store"
f32_store = store "f32.store"
f64_store = store "f64.store"
i32_store8 = store "i32.store8"
i32_store16 = store "i32.store16"
i64_store8 = store "i64.store8"
i64_store16 = store "i64.store16"
i64_store32 = store "i64.store32"

memory_size :: Instr
memory_size =
  Op
    { _opCode = "memory.size"
    , _subExprs = []
    }

memory_grow :: Instr -> Instr
memory_grow subExpr =
  Op
    { _opCode = "memory.grow"
    , _subExprs = [subExpr]
    }
  
i32_const :: Int32 -> Instr
i32_const x =
  ConstOp
    { _literal = B.int32Dec x
    , _constType = I32
    }

i64_const :: Int64 -> Instr
i64_const x =
  ConstOp
    { _literal = B.int64Dec x
    , _constType = I64
    }

f32_const :: Float -> Instr
f32_const x =
  ConstOp
    { _literal = B.floatDec x
    , _constType = F32
    }

f64_const :: Double -> Instr
f64_const x =
  ConstOp
    { _literal = B.doubleDec x
    , _constType = F64
    }

i32_eqz = unop "i32.eqz"
i32_eq = binop "i32.eq"
i32_ne = binop "i32.ne"
i32_lt_s = binop "i32.lt_s"
i32_lt_u = binop "i32.lt_u"
i32_gt_s = binop "i32.gt_s"
i32_gt_u = binop "i32.gt_u"
i32_le_s = binop "i32.le_s"
i32_le_u = binop "i32.le_u"
i32_ge_s = binop "i32.ge_s"
i32_ge_u = binop "i32.ge_u"

i64_eqz = unop "i64.eqz"
i64_eq = binop "i64.eq"
i64_ne = binop "i64.ne"
i64_lt_s = binop "i64.lt_s"
i64_lt_u = binop "i64.lt_u"
i64_gt_s = binop "i64.gt_s"
i64_gt_u = binop "i64.gt_u"
i64_le_s = binop "i64.le_s"
i64_le_u = binop "i64.le_u"
i64_ge_s = binop "i64.ge_s"
i64_ge_u = binop "i64.ge_u"

f32_eq = binop "f32.eq"
f32_ne = binop "f32.ne"
f32_lt = binop "f32.lt"
f32_gt = binop "f32.gt"
f32_le = binop "f32.le"
f32_ge = binop "f32.ge"

f64_eq = binop "f64.eq"
f64_ne = binop "f64.ne"
f64_lt = binop "f64.lt"
f64_gt = binop "f64.gt"
f64_le = binop "f64.le"
f64_ge = binop "f64.ge"

i32_clz = unop "i32.clz"
i32_ctz = unop "i32.ctz"
i32_popcnt = unop "i32.popcnt"
i32_add = binop "i32.add"
i32_sub = binop "i32.sub"
i32_mul = binop "i32.mul"
i32_div_s = binop "i32.div_s"
i32_div_u = binop "i32.div_u"
i32_rem_s = binop "i32.rem_s"
i32_rem_u = binop "i32.rem_u"
i32_and = binop "i32.and"
i32_or = binop "i32.or"
i32_xor = binop "i32.xor"
i32_shl = binop "i32.shl"
i32_shr_s = binop "i32.shr_s"
i32_shr_u = binop "i32.shr_u"
i32_rotl = binop "i32.rotl"
i32_rotr = binop "i32.rotr"

i64_clz = unop "i64.clz"
i64_ctz = unop "i64.ctz"
i64_popcnt = unop "i64.popcnt"
i64_add = binop "i64.add"
i64_sub = binop "i64.sub"
i64_mul = binop "i64.mul"
i64_div_s = binop "i64.div_s"
i64_div_u = binop "i64.div_u"
i64_rem_s = binop "i64.rem_s"
i64_rem_u = binop "i64.rem_u"
i64_and = binop "i64.and"
i64_or = binop "i64.or"
i64_xor = binop "i64.xor"
i64_shl = binop "i64.shl"
i64_shr_s = binop "i64.shr_s"
i64_shr_u = binop "i64.shr_u"
i64_rotl = binop "i64.rotl"
i64_rotr = binop "i64.rotr"

f32_abs = unop "f32.abs"
f32_neg = unop "f32.neg"
f32_ceil = unop "f32.ceil"
f32_floor = unop "f32.floor"
f32_trunc = unop "f32.trunc"
f32_nearest = unop "f32.nearest"
f32_sqrt = unop "f32.sqrt"
f32_add = binop "f32.add"
f32_sub = binop "f32.sub"
f32_mul = binop "f32.mul"
f32_div = binop "f32.div"
f32_min = binop "f32.min"
f32_max = binop "f32.max"
f32_copysign = binop "f32.copysign"

f64_abs = unop "f64.abs"
f64_neg = unop "f64.neg"
f64_ceil = unop "f64.ceil"
f64_floor = unop "f64.floor"
f64_trunc = unop "f64.trunc"
f64_nearest = unop "f64.nearest"
f64_sqrt = unop "f64.sqrt"
f64_add = binop "f64.add"
f64_sub = binop "f64.sub"
f64_mul = binop "f64.mul"
f64_div = binop "f64.div"
f64_min = binop "f64.min"
f64_max = binop "f64.max"
f64_copysign = binop "f64.copysign"

i32_wrap_i64 = unop "i32.wrap/i64"
i32_trunc_s_f32 = unop "i32.trunc_s/f32"
i32_trunc_u_f32 = unop "i32.trunc_u/f32"
i32_trunc_s_f64 = unop "i32.trunc_s/f64"
i32_trunc_u_f64 = unop "i32.trunc_u/f64"
i64_extend_s_i32 = unop "i64.extend_s/i32"
i64_extend_u_i32 = unop "i64.extend_u/i32"
i64_trunc_s_f32 = unop "i64.trunc_s/f32"
i64_trunc_u_f32 = unop "i64.trunc_u/f32"
i64_trunc_s_f64 = unop "i64.trunc_s/f64"
i64_trunc_u_f64 = unop "i64.trunc_u/f64"
f32_convert_s_i32 = unop "f32.convert_s/i32"
f32_convert_u_i32 = unop "f32.convert_u/i32"
f32_convert_s_i64 = unop "f32.convert_s/i64"
f32_convert_u_i64 = unop "f32.convert_u/i64"
f32_demote_f64 = unop "f32.demote/f64"
f64_convert_s_i32 = unop "f64.convert_s/i32"
f64_convert_u_i32 = unop "f64.convert_u/i32"
f64_convert_s_i64 = unop "f64.convert_s/i64"
f64_convert_u_i64 = unop "f64.convert_u/i64"
f64_promote_f32 = unop "f64.promote/f32"
i32_reinterpret_f32 = unop "i32.reinterpret/f32"
i64_reinterpret_f64 = unop "i64.reinterpret/f64"
f32_reinterpret_i32 = unop "f32.reinterpret/i32"
f64_reinterpret_i64 = unop "f64.reinterpret/i64"
