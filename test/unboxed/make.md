Verifying dependencies (0/7)Verifying dependencies (1/7)Verifying dependencies (2/7)Verifying dependencies (3/7)Verifying dependencies (4/7)Verifying dependencies (5/7)Verifying dependencies (6/7)Verifying dependencies (7/7)                            Dependencies ready!
Compiling ...


# MODULE Placeholders

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
  - Let
    Def placeholder0
  - VarOperator +(Basics.add) : forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
    - Call
      - func
        - VarLocal placeholder0
      - args
        - VarLocal a
        - VarLocal b

Declare
Def sum
  - Let
    Def placeholder2
  - VarTopLevel Placeholders.doAdd
    - Let
      Def placeholder1
  - VarForeign List.foldl : forall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
      - Call
        - func
          - VarLocal placeholder1
        - args
          - VarLocal placeholder2
          - Int 0
          - VarLocal list

Declare
Def floatTotal
  - Let
    Def placeholder5
  - VarTopLevel Placeholders.sum
    - Call
      - func
        - VarLocal placeholder5
      - args
        - List
            - Float 1.1
            - Float 2.2
            - Float 3.3

Declare
Def intTotal
  - Let
    Def placeholder4
  - VarTopLevel Placeholders.sum
    - Call
      - func
        - VarLocal placeholder4
      - args
        - VarTopLevel Placeholders.intList

Declare
TypedDef main
  - freeVars:	fromList []
  - type:	TAlias Html Html [(msg,TUnit)] (Holey (TType VirtualDom Node [TVar msg]))
- Call
      - func
        - VarForeign Html.div : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
      - args
        - List
            
        - List
            - Binop
                <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                - Binop
                    ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                    - Str "intTotal = "
                    - Call
                        - func
                          - VarForeign String.fromInt : forall [] => TLambda (TType Basics Int []) (TType String String [])
                        - args
                          - VarTopLevel Placeholders.intTotal
            - Call
                - func
                  - VarForeign Html.br : forall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                - args
                  - List
                      
                  - List
                      
            - Binop
                <|(Basics.apL) : forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                - VarForeign Html.text : forall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                - Binop
                    ++(Basics.append) : forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                    - Str "floatTotal = "
                    - Call
                        - func
                          - VarForeign String.fromFloat : forall [] => TLambda (TType Basics Float []) (TType String String [])
                        - args
                          - VarTopLevel Placeholders.floatTotal

Declare
Def numberTotal
  - Let
    Def placeholder3
  - VarTopLevel Placeholders.sum
    - Call
      - func
        - VarLocal placeholder3
      - args
        - List
            - Int 1
            - Int 2
            - Int 3





# Constraints going into Type.Solve.run

- CLet
  - rigidVars	[]
  - flexVars	[]
  - header	fromList [(intList,AppN List List [AppN Basics Int []])]
  - headerCon
    - CLet
      - rigidVars	[]
      - flexVars	[]
      - header	fromList []
      - headerCon
        - CAnd

      - bodyCon
        - CLet
          - rigidVars	[]
          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
          - header	fromList []
          - headerCon
            - CAnd
              - CAnd
                - CLet
                  - rigidVars	[]
                  - flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  - header	fromList []
                  - headerCon
                    - CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                  - bodyCon
                    - CTrue
                - CLet
                  - rigidVars	[]
                  - flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  - header	fromList []
                  - headerCon
                    - CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                  - bodyCon
                    - CTrue
                - CLet
                  - rigidVars	[]
                  - flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  - header	fromList []
                  - headerCon
                    - CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                  - bodyCon
                    - CTrue
              - CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
          - bodyCon
            - CTrue
  - bodyCon
    - CLet
      - rigidVars	[]
      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
      - header	fromList [(doAdd,FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})))]
      - headerCon
        - CLet
          - rigidVars	[]
          - flexVars	[]
          - header	fromList [(a,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}),(b,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
          - headerCon
            - CAnd

          - bodyCon
            - CLet
              - rigidVars	[]
              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
              - header	fromList [(placeholder0,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              - headerCon
                - CLet
                  - rigidVars	[]
                  - flexVars	[]
                  - header	fromList []
                  - headerCon
                    - CAnd

                  - bodyCon
                    - CForeign +forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
              - bodyCon
                - CLet
                  - rigidVars	[]
                  - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  - header	fromList []
                  - headerCon
                    - CAnd
                      - CLocal placeholder0
                      - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                      - CAnd
                        - CLocal a
                        - CLocal b
                      - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                  - bodyCon
                    - CTrue
      - bodyCon
        - CLet
          - rigidVars	[]
          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
          - header	fromList [(sum,FunN (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}) (VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}))]
          - headerCon
            - CLet
              - rigidVars	[]
              - flexVars	[]
              - header	fromList [(list,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              - headerCon
                - CAnd

              - bodyCon
                - CLet
                  - rigidVars	[]
                  - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  - header	fromList [(placeholder2,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                  - headerCon
                    - CLet
                      - rigidVars	[]
                      - flexVars	[]
                      - header	fromList []
                      - headerCon
                        - CAnd

                      - bodyCon
                        - CLocal doAdd
                  - bodyCon
                    - CLet
                      - rigidVars	[]
                      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      - header	fromList [(placeholder1,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                      - headerCon
                        - CLet
                          - rigidVars	[]
                          - flexVars	[]
                          - header	fromList []
                          - headerCon
                            - CAnd

                          - bodyCon
                            - CForeign foldlforall [a,b] => TLambda (TLambda (TVar a) (TLambda (TVar b) (TVar b))) (TLambda (TVar b) (TLambda (TType List List [TVar a]) (TVar b)))
                      - bodyCon
                        - CLet
                          - rigidVars	[]
                          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                          - header	fromList []
                          - headerCon
                            - CAnd
                              - CLocal placeholder1
                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                              - CAnd
                                - CLocal placeholder2
                                - CLet
                                  - rigidVars	[]
                                  - flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  - header	fromList []
                                  - headerCon
                                    - CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                  - bodyCon
                                    - CTrue
                                - CLocal list
                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                          - bodyCon
                            - CTrue
          - bodyCon
            - CLet
              - rigidVars	[]
              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
              - header	fromList [(floatTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
              - headerCon
                - CLet
                  - rigidVars	[]
                  - flexVars	[]
                  - header	fromList []
                  - headerCon
                    - CAnd

                  - bodyCon
                    - CLet
                      - rigidVars	[]
                      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                      - header	fromList [(placeholder5,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                      - headerCon
                        - CLet
                          - rigidVars	[]
                          - flexVars	[]
                          - header	fromList []
                          - headerCon
                            - CAnd

                          - bodyCon
                            - CLocal sum
                      - bodyCon
                        - CLet
                          - rigidVars	[]
                          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                          - header	fromList []
                          - headerCon
                            - CAnd
                              - CLocal placeholder5
                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                              - CAnd
                                - CLet
                                  - rigidVars	[]
                                  - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  - header	fromList []
                                  - headerCon
                                    - CAnd
                                      - CAnd
                                        - CEqual AppN Basics Float []
                                        - CEqual AppN Basics Float []
                                        - CEqual AppN Basics Float []
                                      - CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  - bodyCon
                                    - CTrue
                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                          - bodyCon
                            - CTrue
              - bodyCon
                - CLet
                  - rigidVars	[]
                  - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                  - header	fromList [(intTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                  - headerCon
                    - CLet
                      - rigidVars	[]
                      - flexVars	[]
                      - header	fromList []
                      - headerCon
                        - CAnd

                      - bodyCon
                        - CLet
                          - rigidVars	[]
                          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                          - header	fromList [(placeholder4,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                          - headerCon
                            - CLet
                              - rigidVars	[]
                              - flexVars	[]
                              - header	fromList []
                              - headerCon
                                - CAnd

                              - bodyCon
                                - CLocal sum
                          - bodyCon
                            - CLet
                              - rigidVars	[]
                              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              - header	fromList []
                              - headerCon
                                - CAnd
                                  - CLocal placeholder4
                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                  - CAnd
                                    - CLocal intList
                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                              - bodyCon
                                - CTrue
                  - bodyCon
                    - CLet
                      - rigidVars	[]
                      - flexVars	[]
                      - header	fromList [(main,AliasN Html Html [(msg,UnitN)] (AppN VirtualDom Node [UnitN]))]
                      - headerCon
                        - CLet
                          - rigidVars	[]
                          - flexVars	[]
                          - header	fromList []
                          - headerCon
                            - CAnd

                          - bodyCon
                            - CLet
                              - rigidVars	[]
                              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                              - header	fromList []
                              - headerCon
                                - CAnd
                                  - CForeign divforall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                  - CAnd
                                    - CLet
                                      - rigidVars	[]
                                      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                      - header	fromList []
                                      - headerCon
                                        - CAnd
                                          - CAnd

                                          - CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                      - bodyCon
                                        - CTrue
                                    - CLet
                                      - rigidVars	[]
                                      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                      - header	fromList []
                                      - headerCon
                                        - CAnd
                                          - CAnd
                                            - CLet
                                              - rigidVars	[]
                                              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                              - header	fromList []
                                              - headerCon
                                                - CAnd
                                                  - CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                                                  - CForeign textforall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                                                  - CLet
                                                    - rigidVars	[]
                                                    - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                    - header	fromList []
                                                    - headerCon
                                                      - CAnd
                                                        - CForeign ++forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                                                        - CEqual AppN String String []
                                                        - CLet
                                                          - rigidVars	[]
                                                          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                          - header	fromList []
                                                          - headerCon
                                                            - CAnd
                                                              - CForeign fromIntforall [] => TLambda (TType Basics Int []) (TType String String [])
                                                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                              - CAnd
                                                                - CLocal intTotal
                                                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                          - bodyCon
                                                            - CTrue
                                                        - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                    - bodyCon
                                                      - CTrue
                                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                              - bodyCon
                                                - CTrue
                                            - CLet
                                              - rigidVars	[]
                                              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                              - header	fromList []
                                              - headerCon
                                                - CAnd
                                                  - CForeign brforall [msg] => TLambda (TType List List [TAlias Html Attribute [(msg,TVar msg)] (Filled (TType VirtualDom Attribute [TVar msg]))]) (TLambda (TType List List [TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))]) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg]))))
                                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                  - CAnd
                                                    - CLet
                                                      - rigidVars	[]
                                                      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                      - header	fromList []
                                                      - headerCon
                                                        - CAnd
                                                          - CAnd

                                                          - CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                      - bodyCon
                                                        - CTrue
                                                    - CLet
                                                      - rigidVars	[]
                                                      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                      - header	fromList []
                                                      - headerCon
                                                        - CAnd
                                                          - CAnd

                                                          - CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                      - bodyCon
                                                        - CTrue
                                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                              - bodyCon
                                                - CTrue
                                            - CLet
                                              - rigidVars	[]
                                              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                              - header	fromList []
                                              - headerCon
                                                - CAnd
                                                  - CForeign <|forall [a,b] => TLambda (TLambda (TVar a) (TVar b)) (TLambda (TVar a) (TVar b))
                                                  - CForeign textforall [msg] => TLambda (TType String String []) (TAlias Html Html [(msg,TVar msg)] (Filled (TType VirtualDom Node [TVar msg])))
                                                  - CLet
                                                    - rigidVars	[]
                                                    - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                    - header	fromList []
                                                    - headerCon
                                                      - CAnd
                                                        - CForeign ++forall [appendable] => TLambda (TVar appendable) (TLambda (TVar appendable) (TVar appendable))
                                                        - CEqual AppN String String []
                                                        - CLet
                                                          - rigidVars	[]
                                                          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                          - header	fromList []
                                                          - headerCon
                                                            - CAnd
                                                              - CForeign fromFloatforall [] => TLambda (TType Basics Float []) (TType String String [])
                                                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                              - CAnd
                                                                - CLocal floatTotal
                                                              - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                          - bodyCon
                                                            - CTrue
                                                        - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                    - bodyCon
                                                      - CTrue
                                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                              - bodyCon
                                                - CTrue
                                          - CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                      - bodyCon
                                        - CTrue
                                  - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                              - bodyCon
                                - CTrue
                      - bodyCon
                        - CLet
                          - rigidVars	[]
                          - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                          - header	fromList [(numberTotal,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                          - headerCon
                            - CLet
                              - rigidVars	[]
                              - flexVars	[]
                              - header	fromList []
                              - headerCon
                                - CAnd

                              - bodyCon
                                - CLet
                                  - rigidVars	[]
                                  - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                  - header	fromList [(placeholder3,VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing})]
                                  - headerCon
                                    - CLet
                                      - rigidVars	[]
                                      - flexVars	[]
                                      - header	fromList []
                                      - headerCon
                                        - CAnd

                                      - bodyCon
                                        - CLocal sum
                                  - bodyCon
                                    - CLet
                                      - rigidVars	[]
                                      - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing},Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                      - header	fromList []
                                      - headerCon
                                        - CAnd
                                          - CLocal placeholder3
                                          - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                          - CAnd
                                            - CLet
                                              - rigidVars	[]
                                              - flexVars	[Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                              - header	fromList []
                                              - headerCon
                                                - CAnd
                                                  - CAnd
                                                    - CLet
                                                      - rigidVars	[]
                                                      - flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                      - header	fromList []
                                                      - headerCon
                                                        - CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                      - bodyCon
                                                        - CTrue
                                                    - CLet
                                                      - rigidVars	[]
                                                      - flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                      - header	fromList []
                                                      - headerCon
                                                        - CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                      - bodyCon
                                                        - CTrue
                                                    - CLet
                                                      - rigidVars	[]
                                                      - flexVars	[Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                                     
actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexSuper	
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexSuper	
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}])

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper Structure

unifyFlexSuperStructure

Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Number
App1 Basics Int []



atomMatchesSuper Number Basics Int

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just b), _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexSuper	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number (Just number), _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Float []
content:Structure (App1 Basics Float [])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Float []
content:Structure (App1 Basics Float [])
otherContent:Structure (App1 Basics Float [])

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Float []
content:Structure (App1 Basics Float [])
otherContent:Structure (App1 Basics Float [])

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}])

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Float []
content:Structure (App1 Basics Float [])
otherContent:FlexSuper Number Nothing

unifyFlexSuperStructure

Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Number
App1 Basics Float []



atomMatchesSuper Number Basics Float

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Float []
content:Structure (App1 Basics Float [])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}])

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Int []
content:Structure (App1 Basics Int [])
otherContent:FlexSuper Number Nothing

unifyFlexSuperStructure

Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Number
App1 Basics Int []



atomMatchesSuper Number Basics Int

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Int []
content:Structure (App1 Basics Int [])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:FlexVar (Just a)

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:FlexSuper Appendable (Just appendable)

unifyFlexSuperStructure

Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Appendable
App1 String String []



atomMatchesSuper Appendable String String

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Int []
content:Structure (App1 Basics Int [])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 20, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 20, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Int []), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Int []
content:Structure (App1 Basics Int [])
otherContent:Structure (App1 Basics Int [])

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:Structure (App1 String String [])

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:Structure (App1 String String [])

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = Alias Html Attribute [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Attribute [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar (Just b), _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar (Just a), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:FlexVar (Just a)

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:FlexSuper Appendable (Just appendable)

unifyFlexSuperStructure

Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Appendable (Just appendable), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Appendable
App1 String String []



atomMatchesSuper Appendable String String

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Float []
content:Structure (App1 Basics Float [])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 16, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 16, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 Basics Float []), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 Basics Float []
content:Structure (App1 Basics Float [])
otherContent:Structure (App1 Basics Float [])

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:Structure (App1 String String [])

unifyStructure 
context:
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 String String []), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 String String []
content:Structure (App1 String String [])
otherContent:Structure (App1 String String [])

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = Alias Html Html [(msg,Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing})] Descriptor {_content = Structure (App1 VirtualDom Node [Descriptor {_content = FlexVar (Just msg), _rank = 1, _mark = Mark 2, _copy = Nothing}]), _rank = 1, _mark = Mark 2, _copy = Nothing}, _rank = 1, _mark = Mark 2, _copy = Nothing}])

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}), _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing}]), _rank = 3, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 3, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:FlexVar Nothing

unifyStructure 
context:
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
content:Structure (Fun1 Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})
otherContent:Structure (Fun1 Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing} Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing})

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}])
otherContent:FlexVar Nothing

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexSuper	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexSuper	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyStructure 
context:
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]), _rank = 2, _mark = Mark 2, _copy = Nothing}
flatType:App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}]
content:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}])
otherContent:Structure (App1 List List [Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}])

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexSuper	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

actuallyUnify FlexSuper
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}

unifyFlexSuper FlexVar	
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexSuper Number Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}
Descriptor {_content = FlexVar Nothing, _rank = 2, _mark = Mark 2, _copy = Nothing}


# Annotations after Type.Solve.run

doAdd	forall [number] => TLambda (TVar number) (TLambda (TVar number) (TVar number))
floatTotal	forall [] => TType Basics Float []
intList	forall [] => TType List List [TType Basics Int []]
intTotal	forall [] => TType Basics Int []
main	forall [] => TAlias Html Html [(msg,TUnit)] (Filled (TType VirtualDom Node [TUnit]))
numberTotal	forall [number] => TVar number
sum	forall [number] => TLambda (TType List List [TVar number]) (TVar number)
 - header	fromList []
                                                      - headerCon
                                                        - CEqual VarN Descriptor {_content = FlexSuper Number Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                                      - bodyCon
                                                        - CTrue
                                                  - CEqual AppN List List [VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}]
                                              - bodyCon
                                                - CTrue
                                          - CEqual VarN Descriptor {_content = FlexVar Nothing, _rank = 0, _mark = Mark 2, _copy = Nothing}
                                      - bodyCon
                                        - CTrue
                          - bodyCon
                            - CSaveTheEnvironment

# Solving...


Compiling (1)Success! Compiled 1 module.

    Placeholders ───> elm.js

