{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.C.FakeAST (mains, graph, g) where
  import qualified Data.Map as Map
  import qualified Data.Set as Set

  import qualified AST.Optimized as Opt
  import qualified Elm.ModuleName as ModuleName
  import qualified Elm.Package as Pkg
  import qualified Data.Name as N
  import qualified Elm.Kernel as K
  
  {-
    outerScopeValue =
      1

    closure arg1 arg2 =
      outerScopeValue + arg1 + arg2

    curried = 
      closure 2

    answer =
      curried 3

  -}


  mains :: Map.Map ModuleName.Canonical Opt.Main
  mains =
    Map.singleton moduleName main


  moduleName :: ModuleName.Canonical
  moduleName =
    ModuleName.Canonical Pkg.dummyName "TestModule"


  main :: Opt.Main
  main =
    Opt.Static


  g :: String -> Opt.Global
  g name =
    Opt.Global moduleName (N.fromChars name)


  l :: String -> Opt.Expr
  l name =
    Opt.VarLocal (N.fromChars name)


  basicsGlobal :: Opt.Global
  basicsGlobal =
    Opt.Global ModuleName.basics (N.fromChars "")


  basicsNode :: Opt.Node
  basicsNode =
    Opt.Kernel [K.JS "// Basics kernel code"] Set.empty


  addGraphKeyDollars :: Opt.Global
  addGraphKeyDollars =
    Opt.Global ModuleName.basics (N.fromChars "add")


  addReferenceDollars :: Opt.Expr
  addReferenceDollars =
    Opt.VarGlobal addGraphKeyDollars


  addReferenceUnderscores :: Opt.Expr
  addReferenceUnderscores =
    Opt.VarKernel N.basics (N.fromChars "add")
  
      

  graph :: Map.Map Opt.Global Opt.Node
  graph =
    Map.fromList
      [ ( basicsGlobal
        , basicsNode
        )
      , ( addGraphKeyDollars
        , Opt.Define addReferenceUnderscores $
            Set.singleton basicsGlobal
        )
      , ( g "outerScopeValue"
        , Opt.Define (Opt.Int 1) Set.empty
        )
      , ( g "closure"
        , Opt.Define
            (Opt.Function
              [N.fromChars "a", N.fromChars "b"]
              (Opt.Call addReferenceDollars
                [ Opt.VarGlobal (g "outerScopeValue")
                , Opt.Call addReferenceDollars
                    [ l "a"
                    , l "b"
                    ]
                ]
              )
            )
            (Set.fromList
              [ g "outerScopeValue"
              , addGraphKeyDollars
              , basicsGlobal
              ])
        )
      , ( g "curried"
        , Opt.Define
            (Opt.Call (Opt.VarGlobal (g "closure")) [Opt.Int 2])
            (Set.singleton $ g "closure")
        )
      , ( g "main"
        , Opt.Define
            (Opt.Call (Opt.VarGlobal (g "curried")) [Opt.Int 3])
            (Set.singleton $ g "curried")
        )
      ]