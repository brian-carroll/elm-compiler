Verifying dependencies (0/7)Verifying dependencies (1/7)Verifying dependencies (2/7)Verifying dependencies (3/7)Verifying dependencies (4/7)Verifying dependencies (5/7)Verifying dependencies (6/7)Verifying dependencies (7/7)                            Dependencies ready!
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
    - TypePlaceholder 9:5-9:6
        - VarLocal a
    - TypePlaceholder 9:9-9:10
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
                  - TypePlaceholder 46:11-46:15
                      - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                  - TypePlaceholder 46:19-46:59
                      - Binop
                        ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                        - TypePlaceholder 46:19-46:32
                            - Str "intTotal = "
                        - TypePlaceholder 46:36-46:59
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
                  - TypePlaceholder 48:11-48:15
                      - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                  - TypePlaceholder 48:19-48:65
                      - Binop
                        ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                        - TypePlaceholder 48:19-48:34
                            - Str "floatTotal = "
                        - TypePlaceholder 48:38-48:65
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





ANNOTATIONS

fromList [(14:16-14:21,forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))),(14:22-14:23,forall [number] => TVar number),(14:24-14:28,forall [number] => TType List List [TVar number]),(14:5-14:15,forall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))),(24:5-24:8,forall [number] => TLambda (TType List List [TVar number]) (TVar number)),(24:9-24:18,forall [number] => TType List List [TVar number]),(30:5-30:14,forall [number] => TType List List [TVar number]),(35:5-35:8,forall [number] => TLambda (TType List List [TVar number]) (TVar number)),(35:9-35:16,forall [] => TType List List [TType Basics Int []]),(40:5-40:8,forall [number] => TLambda (TType List List [TVar number]) (TVar number)),(40:9-40:24,forall [] => TType List List [TType Basics Float []]),(45:5-45:8,forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))),(45:9-45:11,forall [a] => TType List List [TVar a]),(46:11-46:15,forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))),(46:19-46:32,forall [] => TType String String []),(46:19-46:59,forall [] => TType String String []),(46:36-46:50,forall [] => TLambda (TType Basics Int []) (TType String String [])),(46:36-46:59,forall [] => TType String String []),(46:51-46:59,forall [] => TType Basics Int []),(46:9-49:10,forall [msg] => TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]),(47:11-47:13,forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))),(47:14-47:16,forall [a] => TType List List [TVar a]),(47:17-47:19,forall [a] => TType List List [TVar a]),(48:11-48:15,forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))),(48:19-48:34,forall [] => TType String String []),(48:19-48:65,forall [] => TType String String []),(48:38-48:54,forall [] => TLambda (TType Basics Float []) (TType String String [])),(48:38-48:65,forall [] => TType String String []),(48:55-48:65,forall [] => TType Basics Float []),(9:5-9:6,forall [number] => TVar number),(9:9-9:10,forall [] => TVar number),(doAdd,forall [] => TLambda (TVar number) (TLambda (TVar number) (TVar number))),(floatTotal,forall [] => TType Basics Float []),(intList,forall [] => TType List List [TType Basics Int []]),(intTotal,forall [] => TType Basics Int []),(main,forall [] => TAlias Html Html [(msg,TUnit)] (Filled (TType VirtualDom Node [TUnit]))),(numberTotal,forall [number] => TVar number),(sum,forall [] => TLambda (TType List List [TVar number]) (TVar number))]
Compiling (1)Success! Compiled 1 module.
