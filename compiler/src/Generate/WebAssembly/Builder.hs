{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Builder (buildModule) where

import Prelude hiding (lines, id)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Data.Monoid ((<>))

import Generate.WebAssembly.AST
import Generate.WebAssembly.Instructions (i32_const)

-- HELPERS


indent1 :: Builder
indent1 = "  "


indent2 :: Builder
indent2 = indent1 <> indent1


newLineIndent1 :: Builder
newLineIndent1 = "\n" <> indent1


newLineIndent2 :: Builder
newLineIndent2 = "\n" <> indent2


deeper :: Builder -> Builder
deeper indent =
  indent <> indent1


concatWith :: Builder -> [Builder] -> Builder
concatWith separator builderList =
  mconcat $ List.intersperse separator builderList


parens :: Builder -> Builder
parens b =
  "(" <> b <> ")"


-- MODULE

buildModule :: Module -> Builder
buildModule m =
  let
    imports =
      map buildImport (_imports m)

    mem =
      map buildMemory $ maybeToList (_mems m)

    table =
      map buildTable $ maybeToList (_tables m)

    start =
      map buildStart $ maybeToList (_start m)  
  
    decls =
      map buildDeclaration (_decls m)
  in
    parens $
      concatWith newLineIndent1 $
      "module" : imports ++ mem ++ table ++ start ++ decls


buildImport :: Import -> Builder
buildImport (Import level1 level2 descriptor) =
  parens $
    "import "
      <> "\"" <> level1 <> "\" "
      <> "\"" <> level2 <> "\" "
      <> buildDescriptor descriptor


buildMemory :: Memory -> Builder
buildMemory (Memory MemIdxZero limits) =
  parens $
    "memory 0 " <> buildLimits limits


buildTable :: Table -> Builder
buildTable table =
  parens $
    case table of
      TableDeclaration limits AnyFunc ->
        "table 0 " <> buildLimits limits <> " anyfunc"

      TableInlineDef AnyFunc functionIds ->
        "table anyfunc "
          <> buildElementSegment 0 functionIds

      
buildLimits :: Limits -> Builder
buildLimits (Limits initSize maybeMaxSize) =
  let
    maxBuilderList =
      maybeToList $ fmap B.int32Dec maybeMaxSize
  in
    concatWith " " $
      B.int32Dec initSize : maxBuilderList


buildStart :: FunctionId -> Builder
buildStart functionId =
  parens $
    "start " <> buildFunctionId functionId


-- DECLARATIONS


buildDeclaration :: Declaration -> Builder
buildDeclaration decl =
  case decl of
    FuncType maybeTypeId paramTypes maybeResultType ->
      buildFuncType maybeTypeId paramTypes maybeResultType
    
    Global globalId mut valType valueInstr ->
      buildGlobal globalId mut valType valueInstr
    
    ElementSegment offset functionIds ->
      buildElementSegment offset functionIds
    
    DataSegment offset builder ->
      buildDataSegment offset builder
    
    Export jsName descriptor ->
      buildExport jsName descriptor
    
    Function functionId params locals resultType body ->
      buildFunction functionId params locals resultType body


buildFunction :: FunctionId
  -> [(LocalId, ValType)]
  -> [(LocalId, ValType)]
  -> Maybe ValType
  -> [Instr]
  -> Builder
buildFunction functionId params locals resultType body =
  let
    signature =
      concatWith " " $
        "func"
        : buildFunctionId functionId
        : (map (buildSignature "param") params)
        ++ (map (buildSignature "local") locals)
        ++ (map (buildSignatureType "result ") $ maybeToList resultType)

    instrBuilders =
      map (instrToBuilder indent2) body
  in
    parens $
      concatWith newLineIndent2 (signature : instrBuilders)


buildSignature :: Builder -> (LocalId, ValType) -> Builder
buildSignature keyword (localId, valType) =
  parens $
    keyword <> " " <> buildLocalId localId
      <> " " <> buildValType valType


buildFuncType :: Maybe TypeId -> [ValType] -> Maybe ValType -> Builder
buildFuncType maybeTypeId paramTypes maybeResultType =
  let
    typeId = fromMaybe "" (fmap buildTypeId maybeTypeId)

    paramTypeBuilders =
      map (buildSignatureType "param") paramTypes

    resultType =
      maybeToList $
      fmap (buildSignatureType "result") maybeResultType
  in
    parens $ concatWith " " $
      "func" : typeId : paramTypeBuilders ++ resultType


buildSignatureType :: Builder -> ValType -> Builder
buildSignatureType keyword valType =
  parens $
    keyword <> " " <> buildValType valType

    

buildExport :: Builder -> ImportExportDesc -> Builder
buildExport jsName descriptor =
  parens $
    "export "
    <> "\"" <> jsName <> "\" "
    <> buildDescriptor descriptor
        

buildDescriptor :: ImportExportDesc -> Builder
buildDescriptor descriptor =
  parens $
    case descriptor of
      ImpExpFunc functionId -> "func " <> buildFunctionId functionId
      ImpExpTable TableIdxZero -> "table 0"
      ImpExpMem MemIdxZero -> "memory 0"
      ImpExpGlobal globalId -> "global " <> buildGlobalId globalId


buildGlobal :: GlobalId -> Mutability -> ValType -> Instr -> Builder
buildGlobal globalId mut valType valueInstr =
  let
    valueBuilder =
      instrToBuilder "" valueInstr

    typeBuilder =
      case mut of
        Mutable -> "(mut " <> buildValType valType <> ")"
        Immutable -> buildValType valType
  in
    parens $
    "global "
      <> buildGlobalId globalId <> " "
      <> typeBuilder <> " "
      <> valueBuilder


buildDataSegment :: Int32 -> Builder -> Builder
buildDataSegment dataOffset builder =
  let
    dataOffsetBuilder =
      instrToBuilder "" $ i32_const dataOffset
  in
    parens $
      "data "
        <> dataOffsetBuilder
        <> " \""
        <> builder
        <> "\""


buildElementSegment :: Int32 -> [FunctionId] -> Builder
buildElementSegment tableOffset functionIds =
  let
    offsetBuilder =
      instrToBuilder "" (i32_const tableOffset)
  in
      case functionIds of
        [] ->
          ""

        [fid] ->
          parens $
          "elem " <> offsetBuilder <> " " <> buildFunctionId fid

        fids -> 
          parens $
          "elem " <> offsetBuilder <> "\n  "
            <> (concatWith "\n  " $ map buildFunctionId fids)


-- INSTRUCTIONS

instrToBuilder :: Builder -> Instr -> Builder
instrToBuilder indent instr =
  let
    deeperIndent =
      deeper indent

    newLineDeeper =
      "\n" <> deeperIndent

    concatLines builders =
      concatWith newLineDeeper builders

    concatSpaces builders =
      concatWith " " builders

    addParens body =
      "(" <> body <> ")"
  in
    addParens $
      case instr of
        Unreachable ->
          "unreachable"

        Nop ->
          "nop"

        Block labelId valType instrList ->
            buildBlock "block" deeperIndent labelId valType instrList
            <> indent <> "end"

        Loop labelId valType instrList ->
            buildBlock "loop" deeperIndent labelId valType instrList
            <> indent <> "end"

        IfElse valType cond (mThenLabel, thenExpr) (mElseLabel, elseExpr) ->
          let
            condBuilder =
              instrToBuilder deeperIndent cond

            thenLabelList =
              maybeToList $ fmap buildLabelId mThenLabel

            elseLabelList =
              maybeToList $ fmap buildLabelId mElseLabel

            thenBuilder =
              concatLines $
                thenLabelList ++ 
                map (instrToBuilder deeperIndent) thenExpr

            elseBuilder =
              concatLines $
                elseLabelList ++ 
                map (instrToBuilder deeperIndent) elseExpr
          in
            "if " <> (buildValType valType) <> "\n"
              <> deeperIndent <> thenBuilder
              <> indent <> "else\n"
              <> deeperIndent <> elseBuilder
              <> indent <> "end\n"
              <> deeperIndent <> condBuilder

        Br labelId ->
          "br " <> (buildLabelId labelId)

        BrIf labelId condInstr ->
          "br_if " <> buildLabelId labelId
                <> newLineDeeper
                <> instrToBuilder deeperIndent condInstr

        BrTable branchLabels defaultLabel condInstr ->
          let          
            branchBuilders =
              map buildLabelId branchLabels

            condBuilder =
              instrToBuilder deeperIndent condInstr
          in
            "br_table\n"
              <> (concatLines branchBuilders)
              <> newLineDeeper <> (buildLabelId defaultLabel)
              <> newLineDeeper <> condBuilder

        Return ->
          "return"

        Call fid args ->
          concatLines $
            ("call " <> buildFunctionId fid)
            : (map (instrToBuilder deeperIndent) args)


        CallIndirect typeId indexInstr args ->
          concatLines $
            ("call_indirect " <> buildTypeId typeId)
            : (map (instrToBuilder deeperIndent) (indexInstr : args))


        GetLocal localId ->
          "get_local " <> buildLocalId localId


        SetLocal localId value ->
          "set_local " <> buildLocalId localId
            <> newLineDeeper <> instrToBuilder deeperIndent value


        TeeLocal localId value ->
          "tee_local " <> buildLocalId localId
            <> newLineDeeper <> instrToBuilder deeperIndent value


        GetGlobal gid ->
          "get_global " <> buildGlobalId gid


        SetGlobal gid value ->
          "set_global " <> buildGlobalId gid
            <> newLineDeeper <> instrToBuilder deeperIndent value


        Drop droppedInstr ->
          "drop " <> newLineDeeper <> instrToBuilder deeperIndent droppedInstr


        Select instr0 instr1 testInstr ->
          concatLines $
            "select"
            : map (instrToBuilder deeperIndent) [instr0, instr1, testInstr]


        ConstOp valBuilder valType ->
          buildValType valType <> ".const " <> valBuilder


        Op opCodeBuilder operands ->
          concatLines $
            opCodeBuilder : (map (instrToBuilder deeperIndent) operands)


        MemOp opCodeBuilder (MemArg offset memAlign) operands ->
          let
            builders =
              map (instrToBuilder deeperIndent) operands

            firstLine = concatSpaces $
              opCodeBuilder
                : ("offset=" <> B.intDec offset)
                : (buildMemAlign memAlign)
          in
            concatLines $ firstLine : builders


buildBlock :: Builder -> Builder -> LabelId -> ValType -> [Instr] -> Builder
buildBlock opcode deeperIndent labelId valType instrList =
  let
    firstLine =
      opcode
        <> " " <> buildValType valType
        <> " " <> buildLabelId labelId
        <> "\n"

    builders =
      map (instrToBuilder deeperIndent) instrList
  in
    firstLine <>
      (concatWith ("\n" <> deeperIndent) builders)


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


buildTypeId :: TypeId -> Builder
buildTypeId id =
  case id of
    TypeIdx i -> B.intDec i
    TypeName b -> b


buildMemAlign :: MemAlign -> [Builder]
buildMemAlign align =
  case align of
    Align8 -> ["align=3"]
    Align16 -> ["align=4"]
    Align32 -> ["align=5"]
    Align64 -> ["align=6"]
    AlignNatural -> []
