module Generate.WebAssembly.Eval (..) where



data Expr
  = Bool Bool
  | Chr Text
  | Str Text
  | Int Int
  | Float Double
  | VarLocal N.Name
  | VarGlobal Global
  | VarEnum Global Index.ZeroBased
  | VarBox Global
  | VarCycle ModuleName.Canonical N.Name
  | VarDebug N.Name ModuleName.Canonical R.Region (Maybe N.Name)
  | VarKernel N.Name N.Name
  | List [Expr]
  | Function [N.Name] Expr
  | Call Expr [Expr]
  | TailCall N.Name [(N.Name, Expr)]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | Destruct Destructor Expr
  | Case N.Name N.Name (Decider Choice) [(Int, Expr)]
  | Accessor N.Name
  | Access Expr N.Name
  | Update Expr (Map.Map N.Name Expr)
  | Record (Map.Map N.Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Text
