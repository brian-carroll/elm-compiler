module Generate.C.AST where

import qualified Data.ByteString.Builder as B
import qualified Elm.Float as EF


data Ident = Ident B.Builder

data Statement
  = Label Ident Statement
  | Case Expression Statement
  | Cases Expression Expression Statement
  | Default Statement
  | Expr (Maybe Expression)
  | Compound [CompoundBlockItem]
  | If Expression Statement (Maybe Statement)
  | Switch Expression Statement
  | While Expression Statement Bool
  | For (Either (Maybe Expression) Declaration)
      (Maybe Expression)
      (Maybe Expression)
      Statement
  | Goto Ident
  -- | GotoPtr Expression
  | Cont
  | Break
  | Return (Maybe Expression)
  | CommentStatement B.Builder


data Expression
  = Comma       [Expression]         -- (expr1, expr2)
  | Assign      AssignOp                -- assignment operator
                 Expression         -- l-value
                 Expression         -- r-value
  | Cond        Expression         -- conditional
                 Expression         -- true-expression (GNU allows omitting)
                 Expression         -- false-expression
  | Binary      BinaryOp               -- binary operator
                 Expression         -- lhs
                 Expression         -- rhs
  | Cast        Declaration        -- type name
                 Expression
  | Unary       UnaryOp                -- unary operator
                 Expression
  | SizeofExpr  Expression
  | SizeofType  Declaration        -- type name
  | Index       Expression         -- array
                 Expression         -- index
  | Call        Expression         -- function
                 [Expression]         -- arguments
  | Member      Expression         -- structure
                 Ident                   -- member name
                 Bool                    -- deref structure? (True for `->')
  | Var         Ident                   -- identifier (incl. enumeration const)
  | Const       Constant           -- ^ integer, character, floating point and string constants
  | CompoundLit Declaration
                 InitializerList    -- type name & initialiser list
  | StatExpr    Statement        -- ^ GNU C compound statement as expr
  | CommentExpr B.Builder


type InitializerList = [([PartDesignator], Initializer)]

-- | Designators
-- A designator specifies member of an object, either an element or range of an array,
-- or the named member of struct \/ union.
data PartDesignator
  = ArrDesig Expression
  | MemberDesig Ident
  | RangeDesig Expression Expression


-- | C initialization (K&R A8.7, C99 6.7.8)
--
-- Initializers are either assignment expressions or initializer lists
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.
data Initializer
  -- | assignment expression
  = InitExpr Expression
  -- | initialization list (see 'InitList')
  | InitList InitializerList

data Constant
  = IntConst Int
  | CharConst Int
  | FloatConst B.Builder
  | StrConst B.Builder

data Declaration
  = Decl
    [DeclarationSpecifier] -- type specifier and qualifier
    (Maybe Declarator)  -- declarator (may be omitted)
    (Maybe Initializer) -- optional initialize
                            -- annotation
    -- | StaticAssert
    --   Expression         -- assert expression
    --   StringLiteral      -- assert text
                            -- annotation

-- | C declaration specifiers and qualifiers
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
data DeclarationSpecifier
  = TypeSpec    TypeSpecifier    -- ^ type name
  | TypeQual    TypeQualifier    -- ^ type qualifier (const, volatile, register, etc)
  -- = StorageSpec StorageSpecifier -- ^ storage-class specifier or typedef
  -- | FunSpec     FunctionSpecifier -- ^ function specifier (inline or noreturn)
  -- | AlignSpec   AlignmentSpecifier -- ^ alignment specifier

data TypeSpecifier
  = ElmValue
  | ElmInt
  | ElmFloat
  | ElmChar
  | ElmString
  | Cons
  | Tuple2
  | Tuple3
  | Custom
  | Record
  | FieldSet
  | Closure
  | I32
  | F64
  | Void


data TypeQualifier
  = ConstQual
  -- | VolatQual
  -- | RestrQual
  -- | AtomicQual
  -- | AttrQual  Attribute
  -- | NullableQual
  -- | NonnullQual

data Declarator
  = Declr (Maybe Ident) [DerivedDeclarator]

data DerivedDeclarator
  = PtrDeclr [TypeQualifier]
  | ArrDeclr [TypeQualifier] ArraySize
  | FunDeclr [Declaration] -- params

data ArraySize
  = NoArrSize               -- ^ @UnknownSize isCompleteType@
  | ArrSize Expression
  
data CompoundBlockItem
  = BlockStmt Statement    -- ^ A statement
  | BlockDecl Declaration  -- ^ A local declaration

data BinaryOp
  = MulOp
  | DivOp
  | RmdOp                 -- ^ remainder of division
  | AddOp
  | SubOp
  | ShlOp                 -- ^ shift left
  | ShrOp                 -- ^ shift right
  | LtOp                  -- ^ less
  | GtOp                  -- ^ greater
  | LeOp                  -- ^ less or equal
  | GeOp                  -- ^ greater or equal
  | EqOp                  -- ^ equal
  | NeqOp                 -- ^ not equal
  | AndOp                 -- ^ bitwise and
  | XorOp                 -- ^ exclusive bitwise or
  | OrOp                  -- ^ inclusive bitwise or
  | LandOp                -- ^ logical and
  | LorOp                 -- ^ logical or

data UnaryOp
  = PreIncOp               -- ^ prefix increment operator
  | PreDecOp               -- ^ prefix decrement operator
  | PostIncOp              -- ^ postfix increment operator
  | PostDecOp              -- ^ postfix decrement operator
  | AddrOp                 -- ^ address operator
  | DerefOp                -- ^ dereference operator
  | PlusOp                 -- ^ prefix plus
  | MinOp                  -- ^ prefix minus
  | CompOp                 -- ^ one's complement
  | NegOp                  -- ^ logical negation

data AssignOp
  = AssignOp
  | MulAssOp
  | DivAssOp
  | RmdAssOp
  | AddAssOp
  | SubAssOp
  | ShlAssOp
  | ShrAssOp
  | AndAssOp
  | XorAssOp
  | OrAssOp

data FunctionDef =
  FunDef
    [DeclarationSpecifier] -- type specifier and qualifier
    Declarator           -- declarator
    Statement            -- compound statement

data ExternalDeclaration
  = DeclExt Declaration
  | FDefExt FunctionDef
