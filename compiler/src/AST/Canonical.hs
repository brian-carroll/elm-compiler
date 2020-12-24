{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Canonical
  ( Expr, Expr_(..)
  , CaseBranch(..)
  , FieldUpdate(..)
  , CtorOpts(..)
  -- definitions
  , Def(..)
  , Decls(..)
  -- patterns
  , Pattern, Pattern_(..)
  , PatternCtorArg(..)
  -- types
  , Annotation(..)
  , Type(..)
  , AliasType(..)
  , FieldType(..)
  , fieldsToList
  -- modules
  , Module(..)
  , Alias(..)
  , Binop(..)
  , Union(..)
  , Ctor(..)
  , Exports(..)
  , Export(..)
  , Effects(..)
  , Port(..)
  , Manager(..)
  )
  where

{- Creating a canonical AST means finding the home module for all variables.
So if you have L.map, you need to figure out that it is from the elm/core
package in the List module.

In later phases (e.g. type inference, exhaustiveness checking, optimization)
you need to look up additional info from these modules. What is the type?
What are the alternative type constructors? These lookups can be quite costly,
especially in type inference. To reduce costs the canonicalization phase
caches info needed in later phases. This means we no longer build large
dictionaries of metadata with O(log(n)) lookups in those phases. Instead
there is an O(1) read of an existing field! I have tried to mark all
cached data with comments like:

-- CACHE for exhaustiveness
-- CACHE for inference

So it is clear why the data is kept around.
-}


import Control.Monad (liftM, liftM2, liftM3, liftM4, replicateM)
import Data.Binary
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Name (Name)

import qualified AST.Source as Src
import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Float as EF
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Reporting.Annotation as A



-- EXPRESSIONS


type Expr =
  A.Located Expr_


-- CACHE Annotations for type inference
data Expr_
  = VarLocal Name
  | VarTopLevel ModuleName.Canonical Name
  | VarKernel Name Name
  | VarForeign ModuleName.Canonical Name Annotation
  | VarCtor CtorOpts ModuleName.Canonical Name Index.ZeroBased Annotation
  | VarDebug ModuleName.Canonical Name Annotation
  | VarOperator Name ModuleName.Canonical Name Annotation -- CACHE real name for optimization
  | Chr ES.String
  | Str ES.String
  | Int Int
  | Float EF.Float
  | List [Expr]
  | Negate Expr
  | Binop Name ModuleName.Canonical Name Annotation Expr Expr -- CACHE real name for optimization
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | LetRec [Def] Expr
  | LetDestruct Pattern Expr Expr
  | Case Expr [CaseBranch]
  | Accessor Name
  | Access Expr (A.Located Name)
  | Update Name Expr (Map.Map Name FieldUpdate)
  | Record (Map.Map Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Shader.Source Shader.Types
  | UniqueTypeVar Name Expr


data CaseBranch =
  CaseBranch Pattern Expr


data FieldUpdate =
  FieldUpdate A.Region Expr


instance Show Expr_ where
  show e_ = showExpr_ "" e_

showExpr :: String -> Expr -> String
showExpr indent (A.At _ e_) =
  showExpr_ indent e_

showExprs :: String -> [Expr] -> String
showExprs indent exprs =
  List.intercalate ("\n" ++ indent) $ map (showExpr (indent ++ "  ")) exprs

showExpr_ :: String -> Expr_ -> String
showExpr_ indent e =
  let
    nextIndent = indent ++ "  "
    nextIndent2 = indent ++ "    "
  in
  case e of
    VarLocal name -> "- VarLocal " ++ show name
    VarTopLevel moduleName name -> "- VarTopLevel " ++ show moduleName ++ "." ++ show name
    VarKernel kName name -> "- VarKernel " ++ show kName ++ "." ++ show name
    VarForeign moduleName name annotation -> "- VarForeign " ++ show moduleName ++ "." ++ show name ++ " : " ++ show annotation
    VarCtor ctorOpts moduleName name index annotation -> "- VarCtor " ++ show ctorOpts ++ " " ++ show moduleName ++ "." ++ show name ++ "[" ++ (show $ Index.toMachine index) ++ "] : " ++ show annotation
    VarDebug moduleName name annotation -> "- VarDebug " ++ show moduleName ++ "." ++ show name ++ " : " ++ show annotation
    VarOperator name1 moduleName name2 annotation -> "- VarOperator " ++ show name1 ++ "(" ++ show moduleName ++ "." ++ show name2 ++ ") : " ++ show annotation
    Chr eString -> "- Chr '" ++ show eString ++ "'"
    Str eString -> "- Str \"" ++ show eString ++ "\""
    Int int -> "- Int " ++ show int
    Float float -> "- Float " ++ show float
    List exprs -> "- List\n" ++ nextIndent ++ showExprs nextIndent exprs
    Negate expr -> "- Negate " ++ showExpr nextIndent expr
    Binop name1 moduleName name2 annotation expr1 expr2 ->
      "- Binop\n"
      ++ nextIndent ++ show name1 ++ "(" ++ show moduleName ++ "." ++ show name2 ++ ") : " ++ show annotation
      ++ "\n" ++ nextIndent ++ showExpr nextIndent2 expr1
      ++ "\n" ++ nextIndent ++ showExpr nextIndent2 expr2
    Lambda _patterns expr -> "- Lambda\n"
      ++ nextIndent ++ "- patterns\n" -- ++ showIndentedList nextIndent2 patterns
      ++ nextIndent ++ "- expr\n" ++ nextIndent2 ++ showExpr nextIndent2 expr
    Call func args -> "- Call\n"
      ++ nextIndent ++ "- func\n" ++ nextIndent2 ++ showExpr nextIndent2 func ++ "\n"
      ++ nextIndent ++ "- args\n" ++ nextIndent2 ++ showExprs nextIndent2 args
    If condThenPairs elseExpr -> "- If\n"
      ++ (concatMap
          (\(condExpr, thenExpr) -> ""
            ++ nextIndent ++ "- cond\n" ++ nextIndent2 ++ showExpr nextIndent2 condExpr ++ "\n"
            ++ nextIndent ++ "- then\n" ++ nextIndent2 ++ showExpr nextIndent2 thenExpr ++ "\n"
          )
          condThenPairs)
      ++ nextIndent ++ "- else\n" ++ nextIndent2 ++ showExpr nextIndent2 elseExpr
    Let def expr -> "- Let\n"
      ++ nextIndent ++ show def ++ "\n"
      ++ nextIndent ++ showExpr nextIndent expr
    LetRec _defs _expr -> "- LetRec (todo)"
    LetDestruct _pattern _expr1 _expr2 -> "- LetDestruct (todo)"
    Case _expr _caseBranches -> "- Case (todo)"
    Accessor name -> "- Accessor " ++ show name
    Access _expr _name -> "- Access (todo)"
    Update _name _expr _mapFieldUpdates -> "- Update (todo)"
    Record _mapExprs -> "- Record (todo)"
    Unit -> "- Unit"
    Tuple _expr1 _expr2 _mExpr3 -> "- Tuple (todo)"
    Shader _source _types -> "- Shader (todo)"
    UniqueTypeVar name expr -> "- UniqueTypeVar " ++ show name
      ++ "\n" ++ nextIndent ++ showExpr nextIndent expr




-- DEFS


data Def
  = Def (A.Located Name) [Pattern] Expr
  | TypedDef (A.Located Name) FreeVars [(Pattern, Type)] Expr Type

instance Show Def where
  show d =
    case d of
      Def (A.At _ n) _ expr ->
        "Def " ++ show n ++ "\n  " ++ showExpr "  " expr

      TypedDef (A.At _ name) freeVars _ expr tipe ->
        "TypedDef " ++ show name ++ "\n"
          ++ "  - freeVars:\t" ++ show freeVars ++ "\n"
          ++ "  - type:\t" ++ show tipe ++ "\n"
          ++ showExpr "    " expr


-- DECLARATIONS


data Decls
  = Declare Def Decls
  | DeclareRec Def [Def] Decls
  | SaveTheEnvironment


instance Show Decls where
  show ds =
    case ds of 
      Declare def decls -> "\nDeclare\n" ++ show def ++ "\n" ++ show decls
      DeclareRec def defs decls -> "\nDeclareRec\n" ++ show def ++ (List.intercalate "\n\n -" (map show defs)) ++ "\n\n" ++ show decls
      SaveTheEnvironment -> "\n\n"

-- PATTERNS


type Pattern =
  A.Located Pattern_


data Pattern_
  = PAnything
  | PVar Name
  | PRecord [Name]
  | PAlias Pattern Name
  | PUnit
  | PTuple Pattern Pattern (Maybe Pattern)
  | PList [Pattern]
  | PCons Pattern Pattern
  | PBool Union Bool
  | PChr ES.String
  | PStr ES.String
  | PInt Int
  | PCtor
      { _p_home :: ModuleName.Canonical
      , _p_type :: Name
      , _p_union :: Union
      , _p_name :: Name
      , _p_index :: Index.ZeroBased
      , _p_args :: [PatternCtorArg]
      }
      -- CACHE _p_home, _p_type, and _p_vars for type inference
      -- CACHE _p_index to replace _p_name in PROD code gen
      -- CACHE _p_opts to allocate less in PROD code gen
      -- CACHE _p_alts and _p_numAlts for exhaustiveness checker


data PatternCtorArg =
  PatternCtorArg
    { _index :: Index.ZeroBased -- CACHE for destructors/errors
    , _type :: Type             -- CACHE for type inference
    , _arg :: Pattern
    }



-- TYPES


data Annotation = Forall FreeVars Type
  deriving Eq

instance Show Annotation where
  show (Forall freeVars tipe) =
    "forall " ++ show (Map.keys freeVars) ++ " => " ++ show tipe

type FreeVars = Map.Map Name ()


data Type
  = TLambda Type Type
  | TVar Name
  | TType ModuleName.Canonical Name [Type]
  | TRecord (Map.Map Name FieldType) (Maybe Name)
  | TUnit
  | TTuple Type Type (Maybe Type)
  | TAlias ModuleName.Canonical Name [(Name, Type)] AliasType
  deriving (Eq, Show)


data AliasType
  = Holey Type
  | Filled Type
  deriving (Eq, Show)


data FieldType = FieldType {-# UNPACK #-} !Word16 Type
  deriving (Eq, Show)


-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.
--
fieldsToList :: Map.Map Name FieldType -> [(Name, Type)]
fieldsToList fields =
  let
    getIndex (_, FieldType index _) =
      index

    dropIndex (name, FieldType _ tipe) =
      (name, tipe)
  in
  map dropIndex (List.sortOn getIndex (Map.toList fields))



-- MODULES


data Module =
  Module
    { _name    :: ModuleName.Canonical
    , _exports :: Exports
    , _docs    :: Src.Docs
    , _decls   :: Decls
    , _unions  :: Map.Map Name Union
    , _aliases :: Map.Map Name Alias
    , _binops  :: Map.Map Name Binop
    , _effects :: Effects
    }


instance Show Module where
  show (Module name _exports _docs decls _unions _aliases _binops _effects) =
    "\n\n\n# MODULE " ++ show name ++ "\n\n"
    ++ "## decls\n\n" ++ show decls


data Alias = Alias [Name] Type
  deriving (Eq)


data Binop = Binop_ Binop.Associativity Binop.Precedence Name
  deriving (Eq)


data Union =
  Union
    { _u_vars :: [Name]
    , _u_alts :: [Ctor]
    , _u_numAlts :: Int -- CACHE numAlts for exhaustiveness checking
    , _u_opts :: CtorOpts -- CACHE which optimizations are available
    }
  deriving (Eq)


data CtorOpts
  = Normal
  | Enum
  | Unbox
  deriving (Eq, Ord, Show)


data Ctor = Ctor Name Index.ZeroBased Int [Type] -- CACHE length args
  deriving (Eq)



-- EXPORTS


data Exports
  = ExportEverything A.Region
  | Export (Map.Map Name (A.Located Export))


data Export
  = ExportValue
  | ExportBinop
  | ExportAlias
  | ExportUnionOpen
  | ExportUnionClosed
  | ExportPort



-- EFFECTS


data Effects
  = NoEffects
  | Ports (Map.Map Name Port)
  | Manager A.Region A.Region A.Region Manager


data Port
  = Incoming { _freeVars :: FreeVars, _payload :: Type, _func :: Type }
  | Outgoing { _freeVars :: FreeVars, _payload :: Type, _func :: Type }


data Manager
  = Cmd Name
  | Sub Name
  | Fx Name Name



-- BINARY


instance Binary Alias where
  get = liftM2 Alias get get
  put (Alias a b) = put a >> put b


instance Binary Union where
  put (Union a b c d) = put a >> put b >> put c >> put d
  get = liftM4 Union get get get get


instance Binary Ctor where
  get = liftM4 Ctor get get get get
  put (Ctor a b c d) = put a >> put b >> put c >> put d


instance Binary CtorOpts where
  put opts =
    case opts of
      Normal -> putWord8 0
      Enum   -> putWord8 1
      Unbox  -> putWord8 2

  get =
    do  n <- getWord8
        case n of
          0 -> return Normal
          1 -> return Enum
          2 -> return Unbox
          _ -> fail "binary encoding of CtorOpts was corrupted"


instance Binary Annotation where
  get = liftM2 Forall get get
  put (Forall a b) = put a >> put b


instance Binary Type where
  put tipe =
    case tipe of
      TLambda a b        -> putWord8 0 >> put a >> put b
      TVar a             -> putWord8 1 >> put a
      TRecord a b        -> putWord8 2 >> put a >> put b
      TUnit              -> putWord8 3
      TTuple a b c       -> putWord8 4 >> put a >> put b >> put c
      TAlias a b c d     -> putWord8 5 >> put a >> put b >> put c >> put d
      TType home name ts ->
        let potentialWord = length ts + 7 in
        if potentialWord <= fromIntegral (maxBound :: Word8) then
          do  putWord8 (fromIntegral potentialWord)
              put home
              put name
              mapM_ put ts
        else
          putWord8 6 >> put home >> put name >> put ts

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 TLambda get get
          1 -> liftM  TVar get
          2 -> liftM2 TRecord get get
          3 -> return TUnit
          4 -> liftM3 TTuple get get get
          5 -> liftM4 TAlias get get get get
          6 -> liftM3 TType get get get
          n -> liftM3 TType get get (replicateM (fromIntegral (n - 7)) get)


instance Binary AliasType where
  put aliasType =
    case aliasType of
      Holey tipe  -> putWord8 0 >> put tipe
      Filled tipe -> putWord8 1 >> put tipe

  get =
    do  n <- getWord8
        case n of
          0 -> liftM Holey get
          1 -> liftM Filled get
          _ -> fail "binary encoding of AliasType was corrupted"


instance Binary FieldType where
  get = liftM2 FieldType get get
  put (FieldType a b) = put a >> put b
