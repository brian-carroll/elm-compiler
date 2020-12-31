
Verifying dependencies (0/7)
Verifying dependencies (1/7)
Verifying dependencies (2/7)
Verifying dependencies (3/7)
Verifying dependencies (4/7)
Verifying dependencies (5/7)
Verifying dependencies (6/7)
Verifying dependencies (7/7)
                            
Dependencies ready!
Compiling ...


# MODULE Original

## decls


Declare
TypedDef intList
  - freeVars:	fromList []
  - type:	TType List List [TType Basics Int []]
- TypePlaceholder 30:5-30:14
      - List
        - Int 1
        - Int 2
        - Int 3

Declare
Def doAdd
  - Binop
    +(Basics.add) : forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
    - VarLocal a
    - VarLocal b

Declare
Def sum
  - Call
    - func
      - TypePlaceholder 14:5-14:15
        - VarForeign List.foldl : forall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
    - args
      - TypePlaceholder 14:16-14:21
          - VarTopLevel Original.doAdd
      - TypePlaceholder 14:22-14:23
          - Int 0
      - TypePlaceholder 14:24-14:28
          - VarLocal list

Declare
Def floatTotal
  - Call
    - func
      - TypePlaceholder 40:5-40:8
        - VarTopLevel Original.sum
    - args
      - TypePlaceholder 40:9-40:24
          - List
            - Float 1.1
            - Float 2.2
            - Float 3.3

Declare
Def intTotal
  - Call
    - func
      - TypePlaceholder 35:5-35:8
        - VarTopLevel Original.sum
    - args
      - TypePlaceholder 35:9-35:16
          - VarTopLevel Original.intList

Declare
TypedDef main
  - freeVars:	fromList []
  - type:	TAlias Html Html [(msg,TUnit)] (Holey (TType VirtualDom Node [TVar msg]))
- Call
      - func
        - TypePlaceholder 45:5-45:8
          - VarForeign Html.div : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
      - args
        - TypePlaceholder 45:9-45:11
            - List
              
        - TypePlaceholder 46:9-49:10
            - List
              - Binop
                  <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                  - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                  - Binop
                      ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                      - Str "intTotal = "
                      - Call
                          - func
                            - TypePlaceholder 46:36-46:50
                              - VarForeign String.fromInt : forall [] => TLambda (TType Basics Int []) (TType String String [])
                          - args
                            - TypePlaceholder 46:51-46:59
                                - VarTopLevel Original.intTotal
              - Call
                  - func
                    - TypePlaceholder 47:11-47:13
                      - VarForeign Html.br : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                  - args
                    - TypePlaceholder 47:14-47:16
                        - List
                          
                    - TypePlaceholder 47:17-47:19
                        - List
                          
              - Binop
                  <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                  - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                  - Binop
                      ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                      - Str "floatTotal = "
                      - Call
                          - func
                            - TypePlaceholder 48:38-48:54
                              - VarForeign String.fromFloat : forall [] => TLambda (TType Basics Float []) (TType String String [])
                          - args
                            - TypePlaceholder 48:55-48:65
                                - VarTopLevel Original.floatTotal

Declare
Def numberTotal
  - Call
    - func
      - TypePlaceholder 24:5-24:8
        - VarTopLevel Original.sum
    - args
      - TypePlaceholder 24:9-24:18
          - List
            - Int 1
            - Int 2
            - Int 3



fromList [

(14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(14:22-14:23,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}),

(14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(14:5-14:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(24:9-24:18,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(30:5-30:14,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 48, _copy = Nothing}]), _rank = 1, _mark = Mark 28, _copy = Nothing}),

(40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(40:9-40:24,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 48, _copy = Nothing}]), _rank = 1, _mark = Mark 22, _copy = Nothing}),

(45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(45:9-45:11,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(46:36-46:50,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 48, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 48, _copy = Nothing}), _rank = 1, _mark = Mark 48, _copy = Nothing}),

(46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 48, _copy = Nothing}),

(46:9-49:10,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(47:14-47:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(47:17-47:19,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}),

(48:38-48:54,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 48, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 48, _copy = Nothing}), _rank = 1, _mark = Mark 48, _copy = Nothing}),

(48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 48, _copy = Nothing})]

Compiling 

(1)
Success! Compiled 1 module.
