{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Builder
where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Map as Map
import Data.Maybe (maybe, maybeToList)
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Elm.Float as EF

-- import qualified Data.Utf8 as Utf8
import Language.C as C
import Language.C.Data.Name as C
import Language.C.Pretty as C
import qualified Text.PrettyPrint as PP

import Generate.C.AST


-- Language.C AST uses unique integer IDs (confusingly called Name)
-- It's for fast equality, but we're not parsing so we don't need that
dummyNodeId :: C.Name
dummyNodeId =
  C.Name 0


identFromChars :: String -> C.Ident
identFromChars chars =
  C.mkIdent C.nopos chars dummyNodeId


identWithPrefix :: String -> Name.Name -> C.Ident
identWithPrefix prefix name =
  identFromChars (prefix ++ Name.toChars name)


identFromName :: Name.Name -> C.Ident
identFromName name =
  identFromChars (Name.toChars name)


intLiteral :: Int -> CExpr
intLiteral i =
  CConst $ CIntConst (CInteger (fromIntegral i) C.DecRepr C.noFlags) undefNode


intDeclSpec :: CDeclSpec
intDeclSpec = CTypeSpec $ CIntType undefNode


voidDeclSpec :: CDeclSpec
voidDeclSpec = CTypeSpec $ CVoidType undefNode

    
declareEnum :: String -> String -> Set.Set Name.Name -> CExtDecl
declareEnum enumName memberPrefix memberIds =
  let
    enumMembers :: [(C.Ident, Maybe CExpr)]
    enumMembers =
      Set.foldr
        (\memberId idsAndVals ->
          (identWithPrefix memberPrefix memberId, Nothing) : idsAndVals
        )
        []
        memberIds

    fieldEnum :: CEnum
    fieldEnum =
      CEnum
        (Just $ identFromChars enumName)
        (Just $ enumMembers)
        []
        undefNode

    typeSpecifier :: CTypeSpec
    typeSpecifier = CEnumType fieldEnum undefNode

    cDeclarationSpecifier :: CDeclSpec
    cDeclarationSpecifier = CTypeSpec typeSpecifier

    cDeclaration :: CDecl
    cDeclaration = CDecl [cDeclarationSpecifier] [] undefNode
  in
    CDeclExt cDeclaration


join :: B.Builder -> [B.Builder] -> B.Builder
join sep builders =
  mconcat $ List.intersperse sep builders


indent1 :: B.Builder
indent1 = "    "


nIndent1 :: B.Builder
nIndent1 = "\n" <> indent1


fromExpr :: Expression -> B.Builder
fromExpr expression =
  case expression of
    Comma exprList -> "/*Comma*/"
    Assign op lval rval -> "/*Assign*/"
    Cond condition expr1 expr0 -> "/*Cond*/"
    Binary op lhs rhs -> "/*Binary*/"
    Cast typeNameDecl expr -> "/*Cast*/"

    Unary op expr ->
      let e = fromExpr expr in
      case op of
        PreIncOp -> "--" <> e
        PreDecOp -> "--" <> e
        PostIncOp -> e <> "++"
        PostDecOp -> e <> "--"
        AddrOp -> "&" <> e
        DerefOp -> "*" <> e
        PlusOp -> "+" <> e
        MinOp -> "-" <> e
        CompOp -> "~" <> e
        NegOp -> "!" <> e

    SizeofExpr expr -> "/*SizeofExpr*/"
    SizeofType typeNameDecl -> "/*SizeofType*/"
    Index array index ->
      (fromExpr array) <> "[" <> (fromExpr index) <> "]"

    Call funcExpr argExprs ->
      (fromExpr funcExpr)
      <> "("
      <> join ", " (map fromExpr argExprs)
      <> ")"

    Member structure member shouldUseArrow -> "/*Member*/"

    Var (Ident builder) -> builder

    Const constant ->
      case constant of
        IntConst int -> B.intDec int
        CharConst int -> B.intDec int
        FloatConst builder -> builder
        StrConst builder -> "\"" <> builder <> "\""

    CompoundLit initList ->
      fromInitList initList

    StatExpr statement -> "/*StatExpr*/"
    CommentExpr builder -> "/* " <> builder <> " */" 


fromStatement :: B.Builder -> Statement -> B.Builder
fromStatement indent statement =
  let
    nIndent = "\n" <> indent
    deeper = indent <> indent1
    nDeeper = nIndent <> indent1
  in
  case statement of
    Label (Ident ident) statement ->
      ident <> ":" <> nIndent <> (fromStatement deeper statement)

    Case expression statement -> "/* Case */"

    Cases from to statement -> "/* Cases */"

    Default statement ->
      "default:" <> deeper <> fromStatement deeper statement

    Expr maybeExpression -> 
      maybe "" fromExpr maybeExpression

    Compound blockItems ->
      "{" <> nIndent
      <> (join (";\n") $
        map (fromBlockItem deeper) blockItems)
      <> ";" <> nIndent <> "}"

    If condition thenStmt maybeElseStmt  -> "/* If */"
    Switch expression statement  -> "/* Switch */"
    While guardExpr statement isDoWhile -> "/* While */"
    For init guardExpr iterExpr statement -> "/* For */"
    Goto (Ident ident) -> "goto" <> ident
    Cont -> "continue"
    Break -> "break"

    Return maybeExpr ->
      indent <> (join " " $
        "return" : (map fromExpr $ maybeToList maybeExpr)
      )

    CommentStatement builder ->
      "// " <> builder <> ""


fromBlockItem :: B.Builder -> CompoundBlockItem -> B.Builder
fromBlockItem indent item =
  case item of
    BlockStmt statement -> fromStatement indent statement
    BlockDecl declaration -> indent <> (fromDeclaration declaration)


fromDeclaration :: Declaration -> B.Builder
fromDeclaration (Decl declSpecs maybeDeclarator maybeInitializer) =
  (join " " (map fromDeclSpec declSpecs))
  <> " "
  <> (join " = " $
      (map fromDeclarator $ maybeToList maybeDeclarator)
       ++ (map fromInitializer $ maybeToList maybeInitializer)
      )


fromInitializer :: Initializer -> B.Builder
fromInitializer init =
  case init of
    InitExpr expr -> fromExpr expr
    InitList initList -> fromInitList initList


fromInitList :: InitializerList -> B.Builder
fromInitList initList =
  "{"
  <> (join ", " $ map fromInitListItem initList)
  <> "}"


fromInitListItem :: ([PartDesignator], Initializer) -> B.Builder
fromInitListItem (parts, init) =
  (mconcat $ map fromPartDesignator parts)
  <> " = "
  <> fromInitializer init


fromPartDesignator :: PartDesignator -> B.Builder
fromPartDesignator part =
  case part of
    ArrDesig expression ->
      "[" <> fromExpr expression <> "]" -- not sure of this, but prob won't use
    MemberDesig (Ident ident) ->
      "." <> ident
    RangeDesig from to ->
      "[" <> fromExpr from <> "..." <> fromExpr to <> "]" -- not sure of this, but prob won't use


fromDeclSpec :: DeclarationSpecifier -> B.Builder
fromDeclSpec declSpec =
  case declSpec of
    TypeSpec typeSpec -> fromTypeSpecifier typeSpec
    TypeQual typeQual -> fromTypeQualifier typeQual


fromTypeSpecifier :: TypeSpecifier -> B.Builder
fromTypeSpecifier typeSpec =
  case typeSpec of
    ElmValue -> "ElmValue"
    ElmInt -> "ElmInt"
    ElmFloat -> "ElmFloat"
    ElmChar -> "ElmChar"
    ElmString -> "ElmString"
    Cons -> "Cons"
    Tuple2 -> "Tuple2"
    Tuple3 -> "Tuple3"
    Custom -> "Custom"
    Record -> "Record"
    FieldSet -> "FieldSet"
    Closure -> "Closure"
    I32 -> "i32"
    F64 -> "f64"
    Void -> "void"


fromTypeQualifier :: TypeQualifier -> B.Builder
fromTypeQualifier typeQual =
  case typeQual of
    ConstQual -> "const"


fromDeclarator :: Declarator -> B.Builder
fromDeclarator (Declr maybeIdent derivedDeclrs) =
  let
    identBuilder =
      case maybeIdent of
        Nothing -> ""
        Just (Ident ident) -> ident
  in
    List.foldl' fromDerivedDeclr identBuilder derivedDeclrs


fromDerivedDeclr :: B.Builder -> DerivedDeclarator -> B.Builder
fromDerivedDeclr declrBuilder derivedDeclr =
  case derivedDeclr of
    PtrDeclr typeQualifiers ->
      join " " $
        "*" : (map fromTypeQualifier typeQualifiers) ++ [declrBuilder]

    ArrDeclr typeQualifiers arraySize ->
      -- TODO: deal with typeQualifiers? Do I care?
      declrBuilder
        <> "["
        <> (case arraySize of
              NoArrSize -> ""
              ArrSize expr -> fromExpr expr
            )
        <> "]"

    FunDeclr paramDeclarations ->
      declrBuilder
      <> "("
      <> (join ", " $ map fromDeclaration paramDeclarations)
      <> ")"


fromExtDecl :: ExternalDeclaration -> B.Builder
fromExtDecl extDecl =
  case extDecl of
    DeclExt decl ->
      (fromDeclaration decl) <> ";\n"
    FDefExt (FunDef declSpecs declarator statement) ->
      mconcat $
        (map fromDeclSpec declSpecs)
        ++ [fromDeclarator declarator]
        ++ [" ", fromStatement "" statement, "\n"]
