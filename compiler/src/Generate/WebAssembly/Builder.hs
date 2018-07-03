{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Builder (instrToBuilder) where

import Prelude hiding (lines, id)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Data.Monoid ((<>))
import Generate.WebAssembly.AST


-- HELPERS


data Lines = One | Many deriving (Eq)


merge :: Lines -> Lines -> Lines
merge a b =
  if a == Many || b == Many then Many else One


linesMap :: (a -> (Lines, b)) -> [a] -> (Bool, [b])
linesMap func xs =
  let
    pairs =
      map func xs
  in
    ( any ((==) Many . fst) pairs
    , map snd pairs
    )


deeper :: Builder -> Builder
deeper indent =
  "  " <> indent

-- INSTRUCTIONS

instrToBuilder :: Builder -> Instr -> (Lines, Builder)
instrToBuilder indent instr =
  let
    deeperIndent =
      deeper indent

    newLineDeeper =
      "\n" <> deeperIndent

    concatLines builders =
      mconcat $ List.intersperse newLineDeeper builders

    concatSpaces builders =
      mconcat $ List.intersperse " " builders

    (lines, instrBody) =
      case instr of
        Unreachable ->
          (One, "unreachable")

        Nop ->
          (One, "nop")

        Block labelId valType instrList ->
          (,) Many $
            buildBlock "block" deeperIndent labelId valType instrList
            <> indent <> "end"

        Loop labelId valType instrList ->
          (,) Many $
            buildBlock "loop" deeperIndent labelId valType instrList
            <> indent <> "end"

        IfElse valType cond (mThenLabel, thenExpr) (mElseLabel, elseExpr) ->
          let
            (_, condBuilder) =
              instrToBuilder deeperIndent cond

            thenLabelList =
              Maybe.maybeToList $ fmap buildLabelId mThenLabel

            elseLabelList =
              Maybe.maybeToList $ fmap buildLabelId mElseLabel

            thenBuilder =
              concatLines $
                thenLabelList ++ 
                map (snd . instrToBuilder deeperIndent) thenExpr

            elseBuilder =
              concatLines $
                elseLabelList ++ 
                map (snd . instrToBuilder deeperIndent) elseExpr
          in
            (,) Many $
              "if " <> (buildValType valType) <> "\n"
                <> deeperIndent <> thenBuilder
                <> indent <> "else\n"
                <> deeperIndent <> elseBuilder
                <> indent <> "end\n"
                <> deeperIndent <> condBuilder

        Br labelId ->
          (One, "br " <> (buildLabelId labelId))

        BrIf labelId condInstr ->
          let
            (oneOrMany, builder) =
              instrToBuilder deeperIndent condInstr

            sep =
              case oneOrMany of
                One -> " "
                Many -> newLineDeeper
          in
            ( oneOrMany
            , "br_if " <> buildLabelId labelId
                  <> sep <> builder
            )

        BrTable branchLabels defaultLabel condInstr ->
          let          
            branchBuilders =
              map buildLabelId branchLabels

            (_, condBuilder) =
              instrToBuilder deeperIndent condInstr
          in
            (,) Many $
              "br_table\n"
                <> (concatLines branchBuilders)
                <> newLineDeeper <> (buildLabelId defaultLabel)
                <> newLineDeeper <> condBuilder

        Return ->
          (One, "return")

        Call fid args ->
          let
            (anyMany, argBuilders) =
              linesMap (instrToBuilder deeperIndent) args

            firstLine =
              "call " <> buildFunctionId fid
          in
            if anyMany then
              (Many, concatLines $ firstLine : argBuilders)
            else
              (One, concatSpaces $ firstLine : argBuilders)


        CallIndirect fid args ->
          let
            (anyMany, argBuilders) =
              linesMap (instrToBuilder deeperIndent) args

            firstLine =
              "call_indirect " <> buildFunctionId fid
          in
            if anyMany then
              (Many, concatLines $ firstLine : argBuilders)
            else
              (One, concatSpaces $ firstLine : argBuilders)


        GetLocal localId ->
          (One, "get_local " <> buildLocalId localId)


        SetLocal localId value ->
          case instrToBuilder deeperIndent value of
            (One, builder) ->
              (One, "set_local " <> buildLocalId localId <> " " <> builder)
            (Many, builder) ->
              (Many, "set_local " <> buildLocalId localId
                <> newLineDeeper <> builder)


        TeeLocal localId value ->
          case instrToBuilder deeperIndent value of
            (One, builder) ->
              (One, "tee_local " <> buildLocalId localId <> " " <> builder)
            (Many, builder) ->
              (Many, "tee_local " <> buildLocalId localId
                <> newLineDeeper <> builder)


        GetGlobal gid ->
          (One, "get_global " <> buildGlobalId gid)


        SetGlobal gid value ->
          case instrToBuilder deeperIndent value of
            (One, builder) ->
              (One, "set_global " <> buildGlobalId gid <> " " <> builder)
            (Many, builder) ->
              (Many, "set_global " <> buildGlobalId gid
                <> newLineDeeper <> builder)


        Drop droppedInstr ->
          case instrToBuilder deeperIndent droppedInstr of
            (One, builder) ->
              (One, "drop " <> builder)
            (Many, builder) ->
              (Many, "drop " <> newLineDeeper <> builder)


        Select instr0 instr1 testInstr ->
          let
            (anyMany, builders) =
              linesMap
                (instrToBuilder deeperIndent)
                [instr0, instr1, testInstr]
          in
            if anyMany then
              (Many, concatLines $ "select" : builders)
            else
              (One, concatSpaces $ "select" : builders)


        ConstOp valBuilder valType ->
          (,) One $
            buildValType valType <> ".const " <> valBuilder


        Op opCodeBuilder operands ->
          let
            (anyMany, builders) =
              linesMap (instrToBuilder deeperIndent) operands
          in
            if anyMany then
              (Many, concatLines $ opCodeBuilder : builders)
            else
              (One, concatSpaces $ opCodeBuilder : builders)


        MemOp opCodeBuilder (MemArg offset memAlign) operands ->
          let
            (anyMany, builders) =
              linesMap (instrToBuilder deeperIndent) operands

            firstLine = concatSpaces $
              opCodeBuilder
                : ("offset=" <> B.intDec offset)
                : (buildMemAlign memAlign)
          in
            if anyMany then
              (Many, concatLines $ firstLine : builders)
            else
              (One, concatSpaces $ firstLine : builders)
  in
    (lines, "(" <> instrBody <> ")")


buildBlock :: Builder -> Builder -> LabelId -> ValType -> [Instr] -> Builder
buildBlock opcode deeperIndent labelId valType instrList =
  let
    firstLine =
      opcode
        <> " " <> buildValType valType
        <> " " <> buildLabelId labelId
        <> "\n"

    builders =
      map (snd . instrToBuilder deeperIndent) instrList
  in
    firstLine <>
      (mconcat $ List.intersperse ("\n" <> deeperIndent) builders)


buildValType :: ValType -> Builder
buildValType valType =
  case valType of
    I32 -> "i32"
    I64 -> "i64"
    F32 -> "f32"
    F64 -> "f64"


buildLabelId :: LabelId -> Builder
buildLabelId id =
  case id of
    LabelIdx i -> B.intDec i
    LabelName b -> b


buildLocalId :: LocalId -> Builder
buildLocalId id =
  case id of
    LocalIdx i -> B.intDec i
    LocalName b -> b


buildGlobalId :: GlobalId -> Builder
buildGlobalId id =
  case id of
    GlobalIdx i -> B.intDec i
    GlobalName b -> b


buildFunctionId :: FunctionId -> Builder
buildFunctionId id =
  case id of
    FunctionIdx i -> B.intDec i
    FunctionName b -> b


buildMemAlign :: MemAlign -> [Builder]
buildMemAlign align =
  case align of
    Align8 -> ["align=3"]
    Align16 -> ["align=4"]
    Align32 -> ["align=5"]
    Align64 -> ["align=6"]
    AlignNatural -> []
