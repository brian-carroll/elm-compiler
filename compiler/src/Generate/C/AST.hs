module Generate.C.AST where

import qualified Data.ByteString.Builder as B
import qualified Elm.Float as EF
import Generate.C.Name (Name, KernelTypeDef, HeaderFile)
import qualified Generate.C.Name as CN


data Statement
  = Label Name Statement
  | Cases [Expression] Statement
  | Default Statement
  | Expr (Maybe Expression)
  | Compound [CompoundBlockItem] -- reversed decls & statements
  | If Expression Statement (Maybe Statement)
  | Switch Expression Statement
  | While Expression Statement
  | DoWhile Expression Statement
  | For ForInit (Maybe Expression) (Maybe Expression) Statement
  | Goto Name
  | Cont
  | Break
  | Return (Maybe Expression)
  | CommentStatement B.Builder
  | NullStatement


data ForInit
  = ForInitEmpty
  | ForInitExpr Expression
  | ForInitDecl Declaration


data Expression
  = Comma       [Expression]     -- (expr1, expr2)
  | Assign      AssignOp         -- assignment operator
                 Expression      -- l-value
                 Expression      -- r-value
  | Cond        Expression       -- conditional
                 Expression      -- true-expression (GNU allows omitting)
                 Expression      -- false-expression
  | Binary      BinaryOp         -- binary operator
                 Expression      -- lhs
                 Expression      -- rhs
  | Cast        Declaration      -- type name
                 Expression
  | Unary       UnaryOp Expression                 
  | SizeofExpr  Expression
  | SizeofType  Declaration      -- type name
  | Index       Expression       -- array
                 Expression      -- index
  | Call        Expression       -- function
                 [Expression]    -- arguments
  | MemberDot   Expression Name  -- expression.name
  | MemberArrow Expression Name  -- expression->name
  | Var         Name                   -- Name/identifier (incl. enumeration const)
  | Const       Constant           -- ^ integer, character, floating point and string constants
  | CompoundLit InitializerList    -- initialiser list
  | StatExpr    Statement        -- ^ GNU C compound statement as expr
  | Parens      Expression       -- wrap an expr in parentheses
  | CommentExpr B.Builder


type InitializerList = [([PartDesignator], Initializer)]

-- Designators
-- A designator specifies member of an object, either an element or range of an array,
-- or the named member of struct \/ union.
data PartDesignator
  = ArrDesig Expression
  | MemberDesig B.Builder
  | RangeDesig Expression Expression


-- C initialization (K&R A8.7, C99 6.7.8)
--
-- Initializers are either assignment expressions or initializer lists
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.
data Initializer
  = InitExpr Expression
  | InitList InitializerList

data Constant
  = IntConst Int
  | IntHexConst Int
  | CharConst Char
  | FloatConst EF.Float
  | StrConst B.Builder

data Declaration
  = Decl
    [DeclarationSpecifier] -- type specifier and qualifier
    (Maybe Declarator)  -- declarator (may be omitted)
    (Maybe Initializer) -- optional initialize
                            -- annotation
    ---   StaticAssert
    ---   Expression         -- assert expression
    ---   StringLiteral      -- assert text
                            -- annotation

-- C declaration specifiers and qualifiers
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
data DeclarationSpecifier
  = TypeSpec TypeSpecifier    -- ^ type name
  | TypeQual TypeQualifier    -- ^ type qualifier (const, volatile, register, etc)
-- StorageSpec StorageSpecifier -- ^ storage-class specifier or typedef
-- FunSpec     FunctionSpecifier -- ^ function specifier (inline or noreturn)
-- AlignSpec   AlignmentSpecifier -- ^ alignment specifier


data TypeSpecifier
  = TypeDef KernelTypeDef
  | Void
  | Int
  | SizeT
  | Char
  | Enum [Name]


data TypeQualifier
  = ConstQual
--  VolatQual
--  RestrQual
--  AtomicQual
--  AttrQual  Attribute
--  NullableQual
--  NonnullQual

data Declarator
  = Declr (Maybe Name) [DerivedDeclarator]

data DerivedDeclarator
  = PtrDeclr [TypeQualifier]
  | ArrDeclr [TypeQualifier] ArraySize
  | FunDeclr [Declaration]

data ArraySize
  = NoArrSize
  | ArrSize Expression
  
data CompoundBlockItem
  = BlockStmt Statement
  | BlockDecl Declaration

data BinaryOp
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

data UnaryOp
  = PreIncOp   -- prefix increment operator
  | PreDecOp   -- prefix decrement operator
  | PostIncOp  -- postfix increment operator
  | PostDecOp  -- postfix decrement operator
  | AddrOp     -- address operator
  | DerefOp    -- dereference operator
  | PlusOp     -- prefix plus
  | MinOp      -- prefix minus
  | CompOp     -- one's complement
  | NegOp      -- logical negation

data AssignOp
  = AssignOp  --  =
  | MulAssOp  --  *=
  | DivAssOp  --  /=
  | RmdAssOp  --  %=
  | AddAssOp  --  +=
  | SubAssOp  --  -=
  | ShlAssOp  --  <<=
  | ShrAssOp  --  >>=
  | AndAssOp  --  &=
  | XorAssOp  --  ^=
  | OrAssOp   --  |=

data FunctionDef =
  FunDef
    [DeclarationSpecifier] -- type specifier and qualifier
    Declarator           -- declarator
    [CompoundBlockItem]  -- reverse body decls/statements

data ExternalDeclaration
  = DeclExt Declaration
  | FDefExt FunctionDef
  | DefineExt Name Expression
  | IncludeExt HeaderFile
  | CommentExt B.Builder
  | BlankLineExt


-- HELPER FUNCTIONS

arrayLiteral :: TypeSpecifier -> [Expression] -> Expression
arrayLiteral elemType elements =
  Parens $
  Cast
    (Decl [TypeSpec elemType]
      (Just $ Declr Nothing [ArrDeclr [] NoArrSize]) Nothing)
    (CompoundLit $
      map (\elem -> ([], InitExpr elem)) elements)


pointerArray :: [Expression] -> Expression
pointerArray elements =
  Parens $
  Cast
    (Decl [TypeSpec Void]
      (Just $ Declr Nothing [PtrDeclr [], ArrDeclr [] NoArrSize]) Nothing)
    (CompoundLit $
      map (\elem -> ([], InitExpr elem)) elements)


castAsPtrTo :: TypeSpecifier -> Expression -> Expression
castAsPtrTo typespec expr =
  Cast
    (Decl [TypeSpec typespec]
      (Just $ Declr Nothing [PtrDeclr []]) Nothing)
    expr


nameAsVoidPtr :: Name -> Expression
nameAsVoidPtr name =
  castAsPtrTo Void (Var name)


addrOf :: Name -> Expression
addrOf name =
  Unary AddrOp $ Var name


argsArray :: Declaration
argsArray =
  Decl
    [TypeSpec Void]
    (Just $ Declr
      (Just CN.args)
      [PtrDeclr [], ArrDeclr [] NoArrSize])
    Nothing


declare :: Name -> Maybe Expression -> Declaration
declare name mExpr =
  Decl
    [TypeSpec Void]
    (Just $ Declr (Just name) [PtrDeclr []])
    (fmap InitExpr mExpr)
