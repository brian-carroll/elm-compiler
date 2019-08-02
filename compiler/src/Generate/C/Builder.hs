{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Builder
where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Map as Map
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
    Comma exprList -> ""
    Assign op lval rval -> ""
    Cond condition expr1 expr0 -> ""
    Binary op lhs rhs -> ""
    Cast typeNameDecl expr -> ""

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

    SizeofExpr expr -> ""
    SizeofType typeNameDecl -> ""
    Index array index -> ""

    Call funcExpr argExprs ->
      (fromExpr funcExpr)
      <> "("
      <> join ", " (map fromExpr argExprs)
      <> ")"

    Member structure member shouldUseArrow -> ""

    Var (Ident builder) -> builder

    Const constant ->
      case constant of
        IntConst int -> B.intDec int
        CharConst int -> B.intDec int
        FloatConst builder -> builder
        StrConst builder -> "\"" <> builder <> "\""

    CompoundLit typeNameDecl initList -> ""
    StatExpr statement -> ""
    CommentExpr builder -> "/* " <> builder <> " */" 
