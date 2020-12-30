Verifying dependencies (0/7)Verifying dependencies (1/7)Verifying dependencies (2/7)Verifying dependencies (3/7)Verifying dependencies (4/7)Verifying dependencies (5/7)Verifying dependencies (6/7)Verifying dependencies (7/7)                            Dependencies ready!
Compiling ...


# MODULE Original

## decls


Declare
TypedDef intList
  - freeVars:	fromList []
  - type:	TType List List [TType Basics Int []]
- List
      - Int 1
      - Int 2
      - Int 3

Declare
Def doAdd
  - Binop
    +(Basics.add) : forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
    - UniqueTypeVar 9:5-9:6
        - VarLocal a
    - UniqueTypeVar 9:9-9:10
        - VarLocal b

Declare
Def sum
  - Call
    - func
      - VarForeign List.foldl : forall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
    - args
      - UniqueTypeVar 14:16-14:21
          - VarTopLevel Original.doAdd
      - Int 0
      - UniqueTypeVar 14:24-14:28
          - VarLocal list

Declare
Def floatTotal
  - Call
    - func
      - UniqueTypeVar 40:5-40:8
        - VarTopLevel Original.sum
    - args
      - List
          - Float 1.1
          - Float 2.2
          - Float 3.3

Declare
Def intTotal
  - Call
    - func
      - UniqueTypeVar 35:5-35:8
        - VarTopLevel Original.sum
    - args
      - UniqueTypeVar 35:9-35:16
          - VarTopLevel Original.intList

Declare
TypedDef main
  - freeVars:	fromList []
  - type:	TAlias Html Html [(msg,TUnit)] (Holey (TType VirtualDom Node [TVar msg]))
- Call
      - func
        - UniqueTypeVar 45:5-45:8
          - VarForeign Html.div : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
      - args
        - List
            
        - List
            - Binop
                <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                - UniqueTypeVar 46:11-46:15
                    - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                - Binop
                    ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                    - Str "intTotal = "
                    - Call
                        - func
                          - VarForeign String.fromInt : forall [] => TLambda (TType Basics Int []) (TType String String [])
                        - args
                          - UniqueTypeVar 46:51-46:59
                              - VarTopLevel Original.intTotal
            - Call
                - func
                  - UniqueTypeVar 47:11-47:13
                    - VarForeign Html.br : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                - args
                  - List
                      
                  - List
                      
            - Binop
                <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                - UniqueTypeVar 48:11-48:15
                    - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                - Binop
                    ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                    - Str "floatTotal = "
                    - Call
                        - func
                          - VarForeign String.fromFloat : forall [] => TLambda (TType Basics Float []) (TType String String [])
                        - args
                          - UniqueTypeVar 48:55-48:65
                              - VarTopLevel Original.floatTotal

Declare
Def numberTotal
  - Call
    - func
      - UniqueTypeVar 24:5-24:8
        - VarTopLevel Original.sum
    - args
      - List
          - Int 1
          - Int 2
          - Int 3




- solve CLet
  rigidVars	[]
  flexVars	[]
  header	fromList [(intList,AppN List List [AppN Basics Int []])]

  - solve CLet
    rigidVars	[]
    flexVars	[]
    header	fromList []

    - solve CAnd
      
    nextUTV = { }

    - solve CLet
      rigidVars	[]
      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
      header	fromList []

      - solve CAnd
        CAnd
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []
        CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]

        - solve CAnd
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

            - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
            nextUTV = { }
          nextUTV = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

            - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
            nextUTV = { }
          nextUTV = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

            - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
            nextUTV = { }
          nextUTV = { }
        nextUTV = { }

        - solve CEqual AppN List List [VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
        nextUTV = { }
      nextUTV = { }
    nextUTV = { }


   header	{ }
   state1	env
  { }utv
  { }
   locals	{ }
   state2	env
  { }utv
  { }

  nextUTV = { }

  - solve CLet
    rigidVars	[]
    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
    header	fromList [(doAdd,FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})))]

    - solve CLet
      rigidVars	[]
      flexVars	[]
      header	fromList [(a,VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}),(b,VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})]

      - solve CAnd
        
      nextUTV = { }

      - solve CLet
        rigidVars	[]
        flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
        header	fromList []

        - solve CAnd
          CForeign +forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(9:5-9:6,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(9:9-9:10,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
          CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

          - solve CForeign +forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
          answer [ ]
          nextUTV = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(9:5-9:6,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

            - solve CLet
              rigidVars	[]
              flexVars	[]
              header	fromList []

              - solve CAnd
                
              nextUTV = { }

              - solve CLocal a
              answer [ ]
              nextUTV = { }


             header	{ }
             state1	env
            { }utv
            { }
             locals	{ }
             state2	env
            { }utv
            { }

            nextUTV = { }

            - solve CLocal 9:5-9:6
            answer [ ]
            nextUTV = { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}


          rigids	[ ]
          flexs	[ Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}]
          header	{ (9:5-9:6,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          locals	{ (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          newState	env
            { }utv
            { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

          nextUTV = { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(9:9-9:10,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

            - solve CLet
              rigidVars	[]
              flexVars	[]
              header	fromList []

              - solve CAnd
                
              nextUTV = { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

              - solve CLocal b
              answer [ ]
              nextUTV = { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}


             header	{ }
             state1	env
            { }utv
            { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
             locals	{ }
             state2	env
            { }utv
            { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

            nextUTV = { (9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

            - solve CLocal 9:9-9:10
            answer [ ]
            nextUTV = { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}


          rigids	[ ]
          flexs	[ Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}]
          header	{ (9:9-9:10,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          locals	{ (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          newState	env
            { }utv
            { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

          nextUTV = { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

          - solve CEqual VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
          nextUTV = { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
        nextUTV = { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
      nextUTV = { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}


     header	{ (a,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})
    , (b,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
     state1	env
    { }utv
    { }
     locals	{ (a,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})
    , (b,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
     state2	env
    { }utv
    { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

    nextUTV = { (9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}

    - solve CLet
      rigidVars	[]
      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
      header	fromList [(sum,FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}))]

      - solve CLet
        rigidVars	[]
        flexVars	[]
        header	fromList [(list,VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})]

        - solve CAnd
          
        nextUTV = { }

        - solve CLet
          rigidVars	[]
          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
          header	fromList []

          - solve CAnd
            CForeign foldlforall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
            CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
            CAnd
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(14:16-14:21,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(14:24-14:28,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
            CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

            - solve CForeign foldlforall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
            answer [ ]
            nextUTV = { }

            - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
            nextUTV = { }

            - solve CAnd
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(14:16-14:21,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(14:24-14:28,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(14:16-14:21,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                - solve CLet
                  rigidVars	[]
                  flexVars	[]
                  header	fromList []

                  - solve CAnd
                    
                  nextUTV = { }

                  - solve CLocal doAdd
                  answer [ ]
                  nextUTV = { }


                 header	{ }
                 state1	env
                { }utv
                { }
                 locals	{ }
                 state2	env
                { }utv
                { }

                nextUTV = { }

                - solve CLocal 14:16-14:21
                answer [ ]
                nextUTV = { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


              rigids	[ ]
              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	{ (14:16-14:21,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              locals	{ (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              newState	env
                { }utv
                { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              nextUTV = { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []

                - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                nextUTV = { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              nextUTV = { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(14:24-14:28,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                - solve CLet
                  rigidVars	[]
                  flexVars	[]
                  header	fromList []

                  - solve CAnd
                    
                  nextUTV = { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                  - solve CLocal list
                  answer [ ]
                  nextUTV = { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                 header	{ }
                 state1	env
                { }utv
                { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                 locals	{ }
                 state2	env
                { }utv
                { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                nextUTV = { (14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                - solve CLocal 14:24-14:28
                answer [ ]
                nextUTV = { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}


              rigids	[ ]
              flexs	[ Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}]
              header	{ (14:24-14:28,VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
              locals	{ (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
              newState	env
                { }utv
                { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}

              nextUTV = { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
            nextUTV = { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}

            - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
            nextUTV = { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          nextUTV = { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
        nextUTV = { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}


       header	{ (list,VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
       state1	env
      { }utv
      { }
       locals	{ (list,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
       state2	env
      { }utv
      { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}

      nextUTV = { (14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}

      - solve CLet
        rigidVars	[]
        flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
        header	fromList [(floatTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

        - solve CLet
          rigidVars	[]
          flexVars	[]
          header	fromList []

          - solve CAnd
            
          nextUTV = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

            - solve CAnd
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(40:5-40:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
              CAnd
                CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList []
              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(40:5-40:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                - solve CLet
                  rigidVars	[]
                  flexVars	[]
                  header	fromList []

                  - solve CAnd
                    
                  nextUTV = { }

                  - solve CLocal sum
                  answer [ ]
                  nextUTV = { }


                 header	{ }
                 state1	env
                { }utv
                { }
                 locals	{ }
                 state2	env
                { }utv
                { }

                nextUTV = { }

                - solve CLocal 40:5-40:8
                answer [ ]
                nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


              rigids	[ ]
              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	{ (40:5-40:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              locals	{ (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              newState	env
                { }utv
                { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
              nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              - solve CAnd
                CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList []

                - solve CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList []

                  - solve CAnd
                    CAnd
                      CEqual AppN Basics Float []
                      CEqual AppN Basics Float []
                      CEqual AppN Basics Float []
                    CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]

                    - solve CAnd
                      CEqual AppN Basics Float []
                      CEqual AppN Basics Float []
                      CEqual AppN Basics Float []

                      - solve CEqual AppN Basics Float []
                      nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                      - solve CEqual AppN Basics Float []
                      nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                      - solve CEqual AppN Basics Float []
                      nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                    nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    - solve CEqual AppN List List [VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}]
                    nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                  nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              - solve CEqual VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
              nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
            nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
          nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


         header	{ }
         state1	env
        { }utv
        { }
         locals	{ }
         state2	env
        { }utv
        { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

        nextUTV = { (40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

        - solve CLet
          rigidVars	[]
          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
          header	fromList [(intTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

          - solve CLet
            rigidVars	[]
            flexVars	[]
            header	fromList []

            - solve CAnd
              
            nextUTV = { }

            - solve CLet
              rigidVars	[]
              flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	fromList []

              - solve CAnd
                CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList [(35:5-35:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                CAnd
                  CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(35:9-35:16,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

                - solve CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList [(35:5-35:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                  - solve CLet
                    rigidVars	[]
                    flexVars	[]
                    header	fromList []

                    - solve CAnd
                      
                    nextUTV = { }

                    - solve CLocal sum
                    answer [ ]
                    nextUTV = { }


                   header	{ }
                   state1	env
                  { }utv
                  { }
                   locals	{ }
                   state2	env
                  { }utv
                  { }

                  nextUTV = { }

                  - solve CLocal 35:5-35:8
                  answer [ ]
                  nextUTV = { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                rigids	[ ]
                flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	{ (35:5-35:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                locals	{ (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                newState	env
                  { }utv
                  { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                nextUTV = { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
                nextUTV = { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                - solve CAnd
                  CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(35:9-35:16,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                  - solve CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(35:9-35:16,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                    - solve CLet
                      rigidVars	[]
                      flexVars	[]
                      header	fromList []

                      - solve CAnd
                        
                      nextUTV = { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                      - solve CLocal intList
                      answer [ ]
                      nextUTV = { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                     header	{ }
                     state1	env
                    { }utv
                    { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                     locals	{ }
                     state2	env
                    { }utv
                    { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    nextUTV = { (35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    - solve CLocal 35:9-35:16
                    answer [ ]
                    nextUTV = { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}


                  rigids	[ ]
                  flexs	[ Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}]
                  header	{ (35:9-35:16,VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                  locals	{ (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                  newState	env
                    { }utv
                    { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                  nextUTV = { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                nextUTV = { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                - solve CEqual VarN Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                nextUTV = { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}
              nextUTV = { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}
            nextUTV = { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}


           header	{ }
           state1	env
          { }utv
          { }
           locals	{ }
           state2	env
          { }utv
          { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}

          nextUTV = { (35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}

          - solve CLet
            rigidVars	[]
            flexVars	[]
            header	fromList [(main,AliasN Html Html [(msg,UnitN)] (AppN VirtualDom Node [UnitN]))]

            - solve CLet
              rigidVars	[]
              flexVars	[]
              header	fromList []

              - solve CAnd
                
              nextUTV = { }

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []

                - solve CAnd
                  CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(45:5-45:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                  CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
                  CAnd
                    CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList []
                    CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList []
                  CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                  - solve CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(45:5-45:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                    - solve CLet
                      rigidVars	[]
                      flexVars	[]
                      header	fromList []

                      - solve CAnd
                        
                      nextUTV = { }

                      - solve CForeign divforall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                      answer [ ]
                      nextUTV = { }


                     header	{ }
                     state1	env
                    { }utv
                    { }
                     locals	{ }
                     state2	env
                    { }utv
                    { }

                    nextUTV = { }

                    - solve CLocal 45:5-45:8
                    answer [ ]
                    nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                  rigids	[ ]
                  flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	{ (45:5-45:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                  locals	{ (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                  newState	env
                    { }utv
                    { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                  nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                  - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                  nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                  - solve CAnd
                    CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList []
                    CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList []

                    - solve CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList []

                      - solve CAnd
                        CAnd
                          
                        CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]

                        - solve CAnd
                          
                        nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                        - solve CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                        nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                      nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                    nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    - solve CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList []

                      - solve CAnd
                        CAnd
                          CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []
                          CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []
                          CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []
                        CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]

                        - solve CAnd
                          CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []
                          CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []
                          CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []

                          - solve CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []

                            - solve CAnd
                              CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(46:11-46:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList []
                              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                              - solve CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                              answer [ ]
                              nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              - solve CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(46:11-46:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[]
                                  header	fromList []

                                  - solve CAnd
                                    
                                  nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                  - solve CForeign textforall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                                  answer [ ]
                                  nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                                 header	{ }
                                 state1	env
                                { }utv
                                { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                 locals	{ }
                                 state2	env
                                { }utv
                                { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                nextUTV = { (45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                - solve CLocal 46:11-46:15
                                answer [ ]
                                nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                              rigids	[ ]
                              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	{ (46:11-46:15,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              locals	{ (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              newState	env
                                { }utv
                                { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              - solve CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList []

                                - solve CAnd
                                  CForeign ++forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                                  CEqual AppN String String []
                                  CLet
                                    rigidVars	[]
                                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                    header	fromList []
                                  CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                                  - solve CForeign ++forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                                  answer [ ]
                                  nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                  - solve CEqual AppN String String []
                                  nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                  - solve CLet
                                    rigidVars	[]
                                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                    header	fromList []

                                    - solve CAnd
                                      CForeign fromIntforall [] => TLambda (TType Basics Int []) (TType String String [])
                                      CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      CAnd
                                        CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(46:51-46:59,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                                      CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                                      - solve CForeign fromIntforall [] => TLambda (TType Basics Int []) (TType String String [])
                                      answer [ ]
                                      nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                      - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                      - solve CAnd
                                        CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(46:51-46:59,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                        - solve CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(46:51-46:59,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                          - solve CLet
                                            rigidVars	[]
                                            flexVars	[]
                                            header	fromList []

                                            - solve CAnd
                                              
                                            nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                            - solve CLocal intTotal
                                            answer [ ]
                                            nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                                           header	{ }
                                           state1	env
                                          { }utv
                                          { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                           locals	{ }
                                           state2	env
                                          { }utv
                                          { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                          nextUTV = { (46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                          - solve CLocal 46:51-46:59
                                          answer [ ]
                                          nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}


                                        rigids	[ ]
                                        flexs	[ Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                        header	{ (46:51-46:59,VarN Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        locals	{ (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        newState	env
                                          { }utv
                                          { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                        nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                      nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                      - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                    nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                  nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                  - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                  nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                              nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                              - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                            nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                          nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                          - solve CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []

                            - solve CAnd
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(47:11-47:13,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
                              CAnd
                                CLet
                                  rigidVars	[]
                                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  header	fromList []
                                CLet
                                  rigidVars	[]
                                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  header	fromList []
                              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                              - solve CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(47:11-47:13,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[]
                                  header	fromList []

                                  - solve CAnd
                                    
                                  nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                  - solve CForeign brforall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                                  answer [ ]
                                  nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}


                                 header	{ }
                                 state1	env
                                { }utv
                                { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                 locals	{ }
                                 state2	env
                                { }utv
                                { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                nextUTV = { (46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                - solve CLocal 47:11-47:13
                                answer [ ]
                                nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                              rigids	[ ]
                              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	{ (47:11-47:13,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              locals	{ (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              newState	env
                                { }utv
                                { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              - solve CAnd
                                CLet
                                  rigidVars	[]
                                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  header	fromList []
                                CLet
                                  rigidVars	[]
                                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  header	fromList []

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  header	fromList []

                                  - solve CAnd
                                    CAnd
                                      
                                    CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]

                                    - solve CAnd
                                      
                                    nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                    - solve CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                    nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                  nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  header	fromList []

                                  - solve CAnd
                                    CAnd
                                      
                                    CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]

                                    - solve CAnd
                                      
                                    nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                    - solve CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                    nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                  nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                            nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                          nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                          - solve CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []

                            - solve CAnd
                              CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(48:11-48:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList []
                              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                              - solve CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                              answer [ ]
                              nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              - solve CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(48:11-48:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[]
                                  header	fromList []

                                  - solve CAnd
                                    
                                  nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                  - solve CForeign textforall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                                  answer [ ]
                                  nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                                 header	{ }
                                 state1	env
                                { }utv
                                { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                 locals	{ }
                                 state2	env
                                { }utv
                                { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                nextUTV = { (47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                - solve CLocal 48:11-48:15
                                answer [ ]
                                nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                              rigids	[ ]
                              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	{ (48:11-48:15,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              locals	{ (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              newState	env
                                { }utv
                                { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                              - solve CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList []

                                - solve CAnd
                                  CForeign ++forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                                  CEqual AppN String String []
                                  CLet
                                    rigidVars	[]
                                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                    header	fromList []
                                  CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                                  - solve CForeign ++forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                                  answer [ ]
                                  nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                  - solve CEqual AppN String String []
                                  nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                  - solve CLet
                                    rigidVars	[]
                                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                    header	fromList []

                                    - solve CAnd
                                      CForeign fromFloatforall [] => TLambda (TType Basics Float []) (TType String String [])
                                      CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      CAnd
                                        CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(48:55-48:65,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                                      CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                                      - solve CForeign fromFloatforall [] => TLambda (TType Basics Float []) (TType String String [])
                                      answer [ ]
                                      nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                      - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                      - solve CAnd
                                        CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(48:55-48:65,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                        - solve CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(48:55-48:65,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                          - solve CLet
                                            rigidVars	[]
                                            flexVars	[]
                                            header	fromList []

                                            - solve CAnd
                                              
                                            nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                            - solve CLocal floatTotal
                                            answer [ ]
                                            nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                                           header	{ }
                                           state1	env
                                          { }utv
                                          { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                                           locals	{ }
                                           state2	env
                                          { }utv
                                          { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                          nextUTV = { (48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                                          - solve CLocal 48:55-48:65
                                          answer [ ]
                                          nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}


                                        rigids	[ ]
                                        flexs	[ Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                        header	{ (48:55-48:65,VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        locals	{ (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        newState	env
                                          { }utv
                                          { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                        nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                      nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                      - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                    nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                  nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                                  - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                  nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                              nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                              - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                            nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                          nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                        nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                        - solve CEqual AppN List List [VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                        nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                      nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                    nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                  nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                  - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                  nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
              nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}


             header	{ }
             state1	env
            { }utv
            { }
             locals	{ }
             state2	env
            { }utv
            { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

            nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

            - solve CLet
              rigidVars	[]
              flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	fromList [(numberTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

              - solve CLet
                rigidVars	[]
                flexVars	[]
                header	fromList []

                - solve CAnd
                  
                nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                - solve CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList []

                  - solve CAnd
                    CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList [(24:5-24:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                    CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                    CAnd
                      CLet
                        rigidVars	[]
                        flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                        header	fromList []
                    CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

                    - solve CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList [(24:5-24:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                      - solve CLet
                        rigidVars	[]
                        flexVars	[]
                        header	fromList []

                        - solve CAnd
                          
                        nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                        - solve CLocal sum
                        answer [ ]
                        nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}


                       header	{ }
                       state1	env
                      { }utv
                      { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                       locals	{ }
                       state2	env
                      { }utv
                      { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                      nextUTV = { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}

                      - solve CLocal 24:5-24:8
                      answer [ ]
                      nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


                    rigids	[ ]
                    flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	{ (24:5-24:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                    locals	{ (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                    newState	env
                      { }utv
                      { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
                    nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    - solve CAnd
                      CLet
                        rigidVars	[]
                        flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                        header	fromList []

                      - solve CLet
                        rigidVars	[]
                        flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                        header	fromList []

                        - solve CAnd
                          CAnd
                            CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []
                            CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []
                            CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []
                          CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]

                          - solve CAnd
                            CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []
                            CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []
                            CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []

                            - solve CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []

                              - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                              nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                            nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                            - solve CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []

                              - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                              nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                            nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                            - solve CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []

                              - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                              nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                            nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                          nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                          - solve CEqual AppN List List [VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]
                          nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                        nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                      nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                    nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

                    - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                    nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                  nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


               header	{ }
               state1	env
              { }utv
              { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
               locals	{ }
               state2	env
              { }utv
              { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              nextUTV = { (24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

              - solve CSaveTheEnvironment
              nextUTV = { }


            rigids	[ ]
            flexs	[ Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing}]
            header	{ (numberTotal,VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})}
            locals	{ (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})}
            newState	env
              { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
              , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
              , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
              , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}utv
              { }

            nextUTV = { }


           header	{ (main,AliasN Html Html [(msg,UnitN)] (AppN VirtualDom Node [UnitN]))}
           state1	env
          { }utv
          { (48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
           locals	{ (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})}
           state2	env
          { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
          , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
          , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
          , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}utv
          { }

          nextUTV = { }


        rigids	[ ]
        flexs	[ Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
        header	{ (intTotal,VarN Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
        locals	{ (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
        newState	env
          { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
          , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
          , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
          , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}utv
          { }

        nextUTV = { }


      rigids	[ ]
      flexs	[ Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
      header	{ (floatTotal,VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
      locals	{ (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
      newState	env
        { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
        , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
        , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
        , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}utv
        { }

      nextUTV = { }


    rigids	[ ]
    flexs	[ Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}
      , Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
    header	{ (sum,FunN (VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}))}
    locals	{ (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
    newState	env
      { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
      , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
      , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
      , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}utv
      { }

    nextUTV = { }


  rigids	[ ]
  flexs	[ Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}
    , Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}
    , Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}]
  header	{ (doAdd,FunN (VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}) (FunN (VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing})))}
  locals	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
  newState	env
    { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
    , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
    , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
    , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}utv
    { }

  nextUTV = { }


 header	{ (intList,AppN List List [AppN Basics Int []])}
 state1	env
{ }utv
{ }
 locals	{ (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})}
 state2	env
{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
, (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
, (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
, (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
, (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
, (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
, (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}utv
{ }

nextUTV = { }


run UTV

fromList []
Compiling (1)Success! Compiled 1 module.
