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
    - UniqueTypeVar a_9:5-9:6
        - VarLocal a
    - UniqueTypeVar b_9:9-9:10
        - VarLocal b

Declare
Def sum
  - Call
    - func
      - VarForeign List.foldl : forall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
    - args
      - UniqueTypeVar doAdd_14:16-14:21
          - VarTopLevel Original.doAdd
      - Int 0
      - UniqueTypeVar list_14:24-14:28
          - VarLocal list

Declare
Def floatTotal
  - Call
    - func
      - UniqueTypeVar sum_40:5-40:8
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
      - UniqueTypeVar sum_35:5-35:8
        - VarTopLevel Original.sum
    - args
      - UniqueTypeVar intList_35:9-35:16
          - VarTopLevel Original.intList

Declare
TypedDef main
  - freeVars:	fromList []
  - type:	TAlias Html Html [(msg,TUnit)] (Holey (TType VirtualDom Node [TVar msg]))
- Call
      - func
        - UniqueTypeVar div_45:5-45:8
          - VarForeign Html.div : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
      - args
        - List
            
        - List
            - Binop
                <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                - UniqueTypeVar text_46:11-46:15
                    - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                - Binop
                    ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                    - Str "intTotal = "
                    - Call
                        - func
                          - VarForeign String.fromInt : forall [] => TLambda (TType Basics Int []) (TType String String [])
                        - args
                          - UniqueTypeVar intTotal_46:51-46:59
                              - VarTopLevel Original.intTotal
            - Call
                - func
                  - UniqueTypeVar br_47:11-47:13
                    - VarForeign Html.br : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                - args
                  - List
                      
                  - List
                      
            - Binop
                <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                - UniqueTypeVar text_48:11-48:15
                    - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                - Binop
                    ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                    - Str "floatTotal = "
                    - Call
                        - func
                          - VarForeign String.fromFloat : forall [] => TLambda (TType Basics Float []) (TType String String [])
                        - args
                          - UniqueTypeVar floatTotal_48:55-48:65
                              - VarTopLevel Original.floatTotal

Declare
Def numberTotal
  - Call
    - func
      - UniqueTypeVar sum_24:5-24:8
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
      
    nextEnv = { }

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
            nextEnv = { }
          nextEnv = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

            - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
            nextEnv = { }
          nextEnv = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

            - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
            nextEnv = { }
          nextEnv = { }
        nextEnv = { }

        - solve CEqual AppN List List [VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
        nextEnv = { }
      nextEnv = { }
    nextEnv = { }


   header	{ }
   state1	{ }
   locals	{ }
   state2	{ }

  nextEnv = { }

  - solve CLet
    rigidVars	[]
    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
    header	fromList [(doAdd,FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})))]

    - solve CLet
      rigidVars	[]
      flexVars	[]
      header	fromList [(a,VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}),(b,VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})]

      - solve CAnd
        
      nextEnv = { }

      - solve CLet
        rigidVars	[]
        flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
        header	fromList []

        - solve CAnd
          CForeign +forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(a_9:5-9:6,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
          CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(b_9:9-9:10,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
          CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

          - solve CForeign +forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
          answer [ ]
          nextEnv = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(a_9:5-9:6,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

            - solve CLet
              rigidVars	[]
              flexVars	[]
              header	fromList []

              - solve CAnd
                
              nextEnv = { }

              - solve CLocal a
              answer [ ]
              nextEnv = { }


             header	{ }
             state1	{ }
             locals	{ }
             state2	{ }

            nextEnv = { }

            - solve CLocal a_9:5-9:6
            answer [ ]
            nextEnv = { }


          rigids	[ ]
          flexs	[ Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}]
          header	{ (a_9:5-9:6,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          locals	{ (a_9:5-9:6,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          newState	{ }

          nextEnv = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList [(b_9:9-9:10,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

            - solve CLet
              rigidVars	[]
              flexVars	[]
              header	fromList []

              - solve CAnd
                
              nextEnv = { }

              - solve CLocal b
              answer [ ]
              nextEnv = { }


             header	{ }
             state1	{ }
             locals	{ }
             state2	{ }

            nextEnv = { }

            - solve CLocal b_9:9-9:10
            answer [ ]
            nextEnv = { }


          rigids	[ ]
          flexs	[ Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}]
          header	{ (b_9:9-9:10,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          locals	{ (b_9:9-9:10,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
          newState	{ }

          nextEnv = { }

          - solve CEqual VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
          nextEnv = { }
        nextEnv = { }
      nextEnv = { }


     header	{ (a,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})
    , (b,VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
     state1	{ }
     locals	{ (a,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})
    , (b,Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})}
     state2	{ }

    nextEnv = { }

    - solve CLet
      rigidVars	[]
      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
      header	fromList [(sum,FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}))]

      - solve CLet
        rigidVars	[]
        flexVars	[]
        header	fromList [(list,VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})]

        - solve CAnd
          
        nextEnv = { }

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
                header	fromList [(doAdd_14:16-14:21,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(list_14:24-14:28,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
            CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

            - solve CForeign foldlforall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
            answer [ ]
            nextEnv = { }

            - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
            nextEnv = { }

            - solve CAnd
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(doAdd_14:16-14:21,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(list_14:24-14:28,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(doAdd_14:16-14:21,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                - solve CLet
                  rigidVars	[]
                  flexVars	[]
                  header	fromList []

                  - solve CAnd
                    
                  nextEnv = { }

                  - solve CLocal doAdd
                  answer [ ]
                  nextEnv = { }


                 header	{ }
                 state1	{ }
                 locals	{ }
                 state2	{ }

                nextEnv = { }

                - solve CLocal doAdd_14:16-14:21
                answer [ ]
                nextEnv = { }


              rigids	[ ]
              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	{ (doAdd_14:16-14:21,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              locals	{ (doAdd_14:16-14:21,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              newState	{ }

              nextEnv = { }

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []

                - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                nextEnv = { }
              nextEnv = { }

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(list_14:24-14:28,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                - solve CLet
                  rigidVars	[]
                  flexVars	[]
                  header	fromList []

                  - solve CAnd
                    
                  nextEnv = { }

                  - solve CLocal list
                  answer [ ]
                  nextEnv = { }


                 header	{ }
                 state1	{ }
                 locals	{ }
                 state2	{ }

                nextEnv = { }

                - solve CLocal list_14:24-14:28
                answer [ ]
                nextEnv = { }


              rigids	[ ]
              flexs	[ Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}]
              header	{ (list_14:24-14:28,VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
              locals	{ (list_14:24-14:28,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
              newState	{ }

              nextEnv = { }
            nextEnv = { }

            - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
            nextEnv = { }
          nextEnv = { }
        nextEnv = { }


       header	{ (list,VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
       state1	{ }
       locals	{ (list,Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing})}
       state2	{ }

      nextEnv = { }

      - solve CLet
        rigidVars	[]
        flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
        header	fromList [(floatTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

        - solve CLet
          rigidVars	[]
          flexVars	[]
          header	fromList []

          - solve CAnd
            
          nextEnv = { }

          - solve CLet
            rigidVars	[]
            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
            header	fromList []

            - solve CAnd
              CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList [(sum_40:5-40:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
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
                header	fromList [(sum_40:5-40:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                - solve CLet
                  rigidVars	[]
                  flexVars	[]
                  header	fromList []

                  - solve CAnd
                    
                  nextEnv = { }

                  - solve CLocal sum
                  answer [ ]
                  nextEnv = { }


                 header	{ }
                 state1	{ }
                 locals	{ }
                 state2	{ }

                nextEnv = { }

                - solve CLocal sum_40:5-40:8
                answer [ ]
                nextEnv = { }


              rigids	[ ]
              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	{ (sum_40:5-40:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              locals	{ (sum_40:5-40:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
              newState	{ }

              nextEnv = { }

              - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
              nextEnv = { }

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
                      nextEnv = { }

                      - solve CEqual AppN Basics Float []
                      nextEnv = { }

                      - solve CEqual AppN Basics Float []
                      nextEnv = { }
                    nextEnv = { }

                    - solve CEqual AppN List List [VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}]
                    nextEnv = { }
                  nextEnv = { }
                nextEnv = { }
              nextEnv = { }

              - solve CEqual VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
              nextEnv = { }
            nextEnv = { }
          nextEnv = { }


         header	{ }
         state1	{ }
         locals	{ }
         state2	{ }

        nextEnv = { }

        - solve CLet
          rigidVars	[]
          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
          header	fromList [(intTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

          - solve CLet
            rigidVars	[]
            flexVars	[]
            header	fromList []

            - solve CAnd
              
            nextEnv = { }

            - solve CLet
              rigidVars	[]
              flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	fromList []

              - solve CAnd
                CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList [(sum_35:5-35:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                CAnd
                  CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(intList_35:9-35:16,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

                - solve CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList [(sum_35:5-35:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                  - solve CLet
                    rigidVars	[]
                    flexVars	[]
                    header	fromList []

                    - solve CAnd
                      
                    nextEnv = { }

                    - solve CLocal sum
                    answer [ ]
                    nextEnv = { }


                   header	{ }
                   state1	{ }
                   locals	{ }
                   state2	{ }

                  nextEnv = { }

                  - solve CLocal sum_35:5-35:8
                  answer [ ]
                  nextEnv = { }


                rigids	[ ]
                flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	{ (sum_35:5-35:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                locals	{ (sum_35:5-35:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                newState	{ }

                nextEnv = { }

                - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
                nextEnv = { }

                - solve CAnd
                  CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(intList_35:9-35:16,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                  - solve CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(intList_35:9-35:16,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                    - solve CLet
                      rigidVars	[]
                      flexVars	[]
                      header	fromList []

                      - solve CAnd
                        
                      nextEnv = { }

                      - solve CLocal intList
                      answer [ ]
                      nextEnv = { }


                     header	{ }
                     state1	{ }
                     locals	{ }
                     state2	{ }

                    nextEnv = { }

                    - solve CLocal intList_35:9-35:16
                    answer [ ]
                    nextEnv = { }


                  rigids	[ ]
                  flexs	[ Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}]
                  header	{ (intList_35:9-35:16,VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                  locals	{ (intList_35:9-35:16,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                  newState	{ }

                  nextEnv = { }
                nextEnv = { }

                - solve CEqual VarN Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                nextEnv = { }
              nextEnv = { }
            nextEnv = { }


           header	{ }
           state1	{ }
           locals	{ }
           state2	{ }

          nextEnv = { }

          - solve CLet
            rigidVars	[]
            flexVars	[]
            header	fromList [(main,AliasN Html Html [(msg,UnitN)] (AppN VirtualDom Node [UnitN]))]

            - solve CLet
              rigidVars	[]
              flexVars	[]
              header	fromList []

              - solve CAnd
                
              nextEnv = { }

              - solve CLet
                rigidVars	[]
                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                header	fromList []

                - solve CAnd
                  CLet
                    rigidVars	[]
                    flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	fromList [(div_45:5-45:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
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
                    header	fromList [(div_45:5-45:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                    - solve CLet
                      rigidVars	[]
                      flexVars	[]
                      header	fromList []

                      - solve CAnd
                        
                      nextEnv = { }

                      - solve CForeign divforall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                      answer [ ]
                      nextEnv = { }


                     header	{ }
                     state1	{ }
                     locals	{ }
                     state2	{ }

                    nextEnv = { }

                    - solve CLocal div_45:5-45:8
                    answer [ ]
                    nextEnv = { }


                  rigids	[ ]
                  flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	{ (div_45:5-45:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                  locals	{ (div_45:5-45:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                  newState	{ }

                  nextEnv = { }

                  - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                  nextEnv = { }

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
                          
                        nextEnv = { }

                        - solve CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                        nextEnv = { }
                      nextEnv = { }
                    nextEnv = { }

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
                                header	fromList [(text_46:11-46:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList []
                              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                              - solve CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                              answer [ ]
                              nextEnv = { }

                              - solve CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(text_46:11-46:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[]
                                  header	fromList []

                                  - solve CAnd
                                    
                                  nextEnv = { }

                                  - solve CForeign textforall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                                  answer [ ]
                                  nextEnv = { }


                                 header	{ }
                                 state1	{ }
                                 locals	{ }
                                 state2	{ }

                                nextEnv = { }

                                - solve CLocal text_46:11-46:15
                                answer [ ]
                                nextEnv = { }


                              rigids	[ ]
                              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	{ (text_46:11-46:15,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              locals	{ (text_46:11-46:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              newState	{ }

                              nextEnv = { }

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
                                  nextEnv = { }

                                  - solve CEqual AppN String String []
                                  nextEnv = { }

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
                                          header	fromList [(intTotal_46:51-46:59,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                                      CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                                      - solve CForeign fromIntforall [] => TLambda (TType Basics Int []) (TType String String [])
                                      answer [ ]
                                      nextEnv = { }

                                      - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextEnv = { }

                                      - solve CAnd
                                        CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(intTotal_46:51-46:59,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                        - solve CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(intTotal_46:51-46:59,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                          - solve CLet
                                            rigidVars	[]
                                            flexVars	[]
                                            header	fromList []

                                            - solve CAnd
                                              
                                            nextEnv = { }

                                            - solve CLocal intTotal
                                            answer [ ]
                                            nextEnv = { }


                                           header	{ }
                                           state1	{ }
                                           locals	{ }
                                           state2	{ }

                                          nextEnv = { }

                                          - solve CLocal intTotal_46:51-46:59
                                          answer [ ]
                                          nextEnv = { }


                                        rigids	[ ]
                                        flexs	[ Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                        header	{ (intTotal_46:51-46:59,VarN Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        locals	{ (intTotal_46:51-46:59,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        newState	{ }

                                        nextEnv = { }
                                      nextEnv = { }

                                      - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextEnv = { }
                                    nextEnv = { }
                                  nextEnv = { }

                                  - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                  nextEnv = { }
                                nextEnv = { }
                              nextEnv = { }

                              - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextEnv = { }
                            nextEnv = { }
                          nextEnv = { }

                          - solve CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []

                            - solve CAnd
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(br_47:11-47:13,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
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
                                header	fromList [(br_47:11-47:13,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[]
                                  header	fromList []

                                  - solve CAnd
                                    
                                  nextEnv = { }

                                  - solve CForeign brforall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                                  answer [ ]
                                  nextEnv = { }


                                 header	{ }
                                 state1	{ }
                                 locals	{ }
                                 state2	{ }

                                nextEnv = { }

                                - solve CLocal br_47:11-47:13
                                answer [ ]
                                nextEnv = { }


                              rigids	[ ]
                              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	{ (br_47:11-47:13,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              locals	{ (br_47:11-47:13,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              newState	{ }

                              nextEnv = { }

                              - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextEnv = { }

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
                                      
                                    nextEnv = { }

                                    - solve CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                    nextEnv = { }
                                  nextEnv = { }
                                nextEnv = { }

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  header	fromList []

                                  - solve CAnd
                                    CAnd
                                      
                                    CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]

                                    - solve CAnd
                                      
                                    nextEnv = { }

                                    - solve CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                    nextEnv = { }
                                  nextEnv = { }
                                nextEnv = { }
                              nextEnv = { }

                              - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextEnv = { }
                            nextEnv = { }
                          nextEnv = { }

                          - solve CLet
                            rigidVars	[]
                            flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                            header	fromList []

                            - solve CAnd
                              CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(text_48:11-48:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                              CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList []
                              CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                              - solve CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                              answer [ ]
                              nextEnv = { }

                              - solve CLet
                                rigidVars	[]
                                flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                header	fromList [(text_48:11-48:15,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                - solve CLet
                                  rigidVars	[]
                                  flexVars	[]
                                  header	fromList []

                                  - solve CAnd
                                    
                                  nextEnv = { }

                                  - solve CForeign textforall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                                  answer [ ]
                                  nextEnv = { }


                                 header	{ }
                                 state1	{ }
                                 locals	{ }
                                 state2	{ }

                                nextEnv = { }

                                - solve CLocal text_48:11-48:15
                                answer [ ]
                                nextEnv = { }


                              rigids	[ ]
                              flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	{ (text_48:11-48:15,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              locals	{ (text_48:11-48:15,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                              newState	{ }

                              nextEnv = { }

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
                                  nextEnv = { }

                                  - solve CEqual AppN String String []
                                  nextEnv = { }

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
                                          header	fromList [(floatTotal_48:55-48:65,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                                      CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

                                      - solve CForeign fromFloatforall [] => TLambda (TType Basics Float []) (TType String String [])
                                      answer [ ]
                                      nextEnv = { }

                                      - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextEnv = { }

                                      - solve CAnd
                                        CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(floatTotal_48:55-48:65,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                        - solve CLet
                                          rigidVars	[]
                                          flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                          header	fromList [(floatTotal_48:55-48:65,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                                          - solve CLet
                                            rigidVars	[]
                                            flexVars	[]
                                            header	fromList []

                                            - solve CAnd
                                              
                                            nextEnv = { }

                                            - solve CLocal floatTotal
                                            answer [ ]
                                            nextEnv = { }


                                           header	{ }
                                           state1	{ }
                                           locals	{ }
                                           state2	{ }

                                          nextEnv = { }

                                          - solve CLocal floatTotal_48:55-48:65
                                          answer [ ]
                                          nextEnv = { }


                                        rigids	[ ]
                                        flexs	[ Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
                                        header	{ (floatTotal_48:55-48:65,VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        locals	{ (floatTotal_48:55-48:65,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
                                        newState	{ }

                                        nextEnv = { }
                                      nextEnv = { }

                                      - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                      nextEnv = { }
                                    nextEnv = { }
                                  nextEnv = { }

                                  - solve CEqual VarN Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
                                  nextEnv = { }
                                nextEnv = { }
                              nextEnv = { }

                              - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                              nextEnv = { }
                            nextEnv = { }
                          nextEnv = { }
                        nextEnv = { }

                        - solve CEqual AppN List List [VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]
                        nextEnv = { }
                      nextEnv = { }
                    nextEnv = { }
                  nextEnv = { }

                  - solve CEqual VarN Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
                  nextEnv = { }
                nextEnv = { }
              nextEnv = { }


             header	{ }
             state1	{ }
             locals	{ }
             state2	{ }

            nextEnv = { }

            - solve CLet
              rigidVars	[]
              flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
              header	fromList [(numberTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

              - solve CLet
                rigidVars	[]
                flexVars	[]
                header	fromList []

                - solve CAnd
                  
                nextEnv = { }

                - solve CLet
                  rigidVars	[]
                  flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  header	fromList []

                  - solve CAnd
                    CLet
                      rigidVars	[]
                      flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      header	fromList [(sum_24:5-24:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
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
                      header	fromList [(sum_24:5-24:8,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]

                      - solve CLet
                        rigidVars	[]
                        flexVars	[]
                        header	fromList []

                        - solve CAnd
                          
                        nextEnv = { }

                        - solve CLocal sum
                        answer [ ]
                        nextEnv = { }


                       header	{ }
                       state1	{ }
                       locals	{ }
                       state2	{ }

                      nextEnv = { }

                      - solve CLocal sum_24:5-24:8
                      answer [ ]
                      nextEnv = { }


                    rigids	[ ]
                    flexs	[ Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}]
                    header	{ (sum_24:5-24:8,VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                    locals	{ (sum_24:5-24:8,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
                    newState	{ }

                    nextEnv = { }

                    - solve CEqual VarN Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
                    nextEnv = { }

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
                              nextEnv = { }
                            nextEnv = { }

                            - solve CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []

                              - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                              nextEnv = { }
                            nextEnv = { }

                            - solve CLet
                              rigidVars	[]
                              flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              header	fromList []

                              - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                              nextEnv = { }
                            nextEnv = { }
                          nextEnv = { }

                          - solve CEqual AppN List List [VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]
                          nextEnv = { }
                        nextEnv = { }
                      nextEnv = { }
                    nextEnv = { }

                    - solve CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
                    nextEnv = { }
                  nextEnv = { }
                nextEnv = { }


               header	{ }
               state1	{ }
               locals	{ }
               state2	{ }

              nextEnv = { }

              - solve CSaveTheEnvironment
              nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
              , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
              , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
              , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


            rigids	[ ]
            flexs	[ Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing}]
            header	{ (numberTotal,VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})}
            locals	{ (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})}
            newState	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
              , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
              , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
              , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
              , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

            nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
            , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
            , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
            , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
            , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
            , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
            , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


           header	{ (main,AliasN Html Html [(msg,UnitN)] (AppN VirtualDom Node [UnitN]))}
           state1	{ }
           locals	{ (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})}
           state2	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
          , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
          , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
          , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

          nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
          , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
          , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
          , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


        rigids	[ ]
        flexs	[ Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
        header	{ (intTotal,VarN Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
        locals	{ (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
        newState	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
          , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
          , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
          , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
          , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

        nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
        , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
        , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
        , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


      rigids	[ ]
      flexs	[ Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
      header	{ (floatTotal,VarN Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
      locals	{ (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})}
      newState	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
        , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
        , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
        , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
        , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

      nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
      , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
      , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
      , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


    rigids	[ ]
    flexs	[ Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}
      , Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
    header	{ (sum,FunN (VarN Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}))}
    locals	{ (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
    newState	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
      , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
      , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
      , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
      , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

    nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
    , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
    , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
    , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


  rigids	[ ]
  flexs	[ Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}
    , Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}
    , Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}]
  header	{ (doAdd,FunN (VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}) (FunN (VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing})))}
  locals	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
  newState	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
    , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
    , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
    , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
    , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

  nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
  , (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
  , (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
  , (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
  , (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
  , (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
  , (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}


 header	{ (intList,AppN List List [AppN Basics Int []])}
 state1	{ }
 locals	{ (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})}
 state2	{ (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
, (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
, (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
, (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
, (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
, (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
, (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}

nextEnv = { (doAdd,Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})
, (floatTotal,Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing})
, (intList,Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 24, _copy = Nothing})
, (intTotal,Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing})
, (main,Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
, (numberTotal,Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 40, _copy = Nothing})
, (sum,Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]), _rank = 0, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}), _rank = 0, _mark = Mark 2, _copy = Nothing})}
Compiling (1)Success! Compiled 1 module.

    Original ───> orig.js

