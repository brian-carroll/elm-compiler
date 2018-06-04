port module BrianPlayground exposing (elmToJs, jsToElm, view)

{-| Playing around to understand Elm AST & code gen
-}

import Html exposing (..)


port elmToJs : JsInterface -> Cmd msg


port jsToElm : (JsInterface -> msg) -> Sub msg


type alias JsInterface =
    String


view =
    div []
        [ ul [] <|
            List.map
                (\(MyBox n) -> li [] [ text <| String.fromInt n ])
                [ MyBox 1
                , MyBox 2
                , MyBox 3
                , MyBox 4
                , MyBox 5
                ]
        , mutualRecursionTest1 0
        , viewEnumList
        , viewIf True False
        , ul [] <| List.map (\_ -> li [] []) stupidList
        , viewExtRecord exampleRecord1
        , viewExtRecord exampleRecord2
        ]


type alias MyRecord1 =
    { thisField : Int
    , thatField : Int
    , theOtherField : Int
    }


type alias MyRecord2 =
    { thisField : Int
    , thatField : Int
    }


exampleRecord1 : MyRecord1
exampleRecord1 =
    { thisField = 1
    , thatField = 2
    , theOtherField = 3
    }


exampleRecord2 : MyRecord2
exampleRecord2 =
    { thisField = 1
    , thatField = 2
    }


viewExtRecord : { b | thisField : Int } -> Html msg
viewExtRecord r =
    r
        |> extRecordFun
        |> .thisField
        |> String.fromInt
        |> text


extRecordFun : { b | thisField : Int } -> { b | thisField : Int }
extRecordFun r =
    { r
        | thisField = r.thisField + 1
    }


{-| TailCall is not emitted for mutual recursion
-}
mutualRecursionTest1 : Int -> Html msg
mutualRecursionTest1 x =
    if x < 10 then
        mutualRecursionTest2 (x + 1)

    else
        text "That's it"


mutualRecursionTest2 : Int -> Html msg
mutualRecursionTest2 x =
    if x < 10 then
        mutualRecursionTest1 (x + 1)

    else
        text "That's it"


viewIf x y =
    if x && y then
        text "first"

    else if y then
        text "second"

    else
        text "third"


type TestBox
    = MyBox Int


type TestEnum
    = Banana
    | Orange Int
    | Apple


viewEnumList =
    div [] <|
        List.map viewEnum [ Banana, Orange 5, Apple ]


viewEnum : TestEnum -> Html msg
viewEnum thing =
    case thing of
        Banana ->
            text "Banana"

        Orange x ->
            text ("Orange" ++ String.fromInt x)

        Apple ->
            text "Apple"


type MassiveUnion
    = X00
    | X01
    | X02
    | X03
    | X04
    | X05
    | X06
    | X07
    | X08
    | X09
    | X0a
    | X0b
    | X0c
    | X0d
    | X0e
    | X0f
      --
    | X10
    | X11
    | X12
    | X13
    | X14
    | X15
    | X16
    | X17
    | X18
    | X19
    | X1a
    | X1b
    | X1c
    | X1d
    | X1e
    | X1f
      --
    | X20
    | X21
    | X22
    | X23
    | X24
    | X25
    | X26
    | X27
    | X28
    | X29
    | X2a
    | X2b
    | X2c
    | X2d
    | X2e
    | X2f
      --
    | X30
    | X31
    | X32
    | X33
    | X34
    | X35
    | X36
    | X37
    | X38
    | X39
    | X3a
    | X3b
    | X3c
    | X3d
    | X3e
    | X3f
      --
    | X40
    | X41
    | X42
    | X43
    | X44
    | X45
    | X46
    | X47
    | X48
    | X49
    | X4a
    | X4b
    | X4c
    | X4d
    | X4e
    | X4f
      --
    | X50
    | X51
    | X52
    | X53
    | X54
    | X55
    | X56
    | X57
    | X58
    | X59
    | X5a
    | X5b
    | X5c
    | X5d
    | X5e
    | X5f
      --
    | X60
    | X61
    | X62
    | X63
    | X64
    | X65
    | X66
    | X67
    | X68
    | X69
    | X6a
    | X6b
    | X6c
    | X6d
    | X6e
    | X6f
      --
    | X70
    | X71
    | X72
    | X73
    | X74
    | X75
    | X76
    | X77
    | X78
    | X79
    | X7a
    | X7b
    | X7c
    | X7d
    | X7e
    | X7f
      {--}
    | X100
    | X101
    | X102
    | X103
    | X104
    | X105
    | X106
    | X107
    | X108
    | X109
    | X10a
    | X10b
    | X10c
    | X10d
    | X10e
    | X10f
      --
    | X110
    | X111
    | X112
    | X113
    | X114
    | X115
    | X116
    | X117
    | X118
    | X119
    | X11a
    | X11b
    | X11c
    | X11d
    | X11e
    | X11f
      --
    | X120
    | X121
    | X122
    | X123
    | X124
    | X125
    | X126
    | X127
    | X128
    | X129
    | X12a
    | X12b
    | X12c
    | X12d
    | X12e
    | X12f
      --
    | X130
    | X131
    | X132
    | X133
    | X134
    | X135
    | X136
    | X137
    | X138
    | X139
    | X13a
    | X13b
    | X13c
    | X13d
    | X13e
    | X13f
      --
    | X140
    | X141
    | X142
    | X143
    | X144
    | X145
    | X146
    | X147
    | X148
    | X149
    | X14a
    | X14b
    | X14c
    | X14d
    | X14e
    | X14f
      --
    | X150
    | X151
    | X152
    | X153
    | X154
    | X155
    | X156
    | X157
    | X158
    | X159
    | X15a
    | X15b
    | X15c
    | X15d
    | X15e
    | X15f
      --
    | X160
    | X161
    | X162
    | X163
    | X164
    | X165
    | X166
    | X167
    | X168
    | X169
    | X16a
    | X16b
    | X16c
    | X16d
    | X16e
    | X16f
      --
    | X170
    | X171
    | X172
    | X173
    | X174
    | X175
    | X176
    | X177
    | X178
    | X179
    | X17a
    | X17b
    | X17c
    | X17d
    | X17e
    | X17f
      --
    | X200


stupidList =
    [ X00
    , X01
    , X02
    , X03
    , X04
    , X05
    , X06
    , X07
    , X08
    , X09
    , X0a
    , X0b
    , X0c
    , X0d
    , X0e
    , X0f

    --
    , X10
    , X11
    , X12
    , X13
    , X14
    , X15
    , X16
    , X17
    , X18
    , X19
    , X1a
    , X1b
    , X1c
    , X1d
    , X1e
    , X1f

    --
    , X20
    , X21
    , X22
    , X23
    , X24
    , X25
    , X26
    , X27
    , X28
    , X29
    , X2a
    , X2b
    , X2c
    , X2d
    , X2e
    , X2f

    --
    , X30
    , X31
    , X32
    , X33
    , X34
    , X35
    , X36
    , X37
    , X38
    , X39
    , X3a
    , X3b
    , X3c
    , X3d
    , X3e
    , X3f

    --
    , X40
    , X41
    , X42
    , X43
    , X44
    , X45
    , X46
    , X47
    , X48
    , X49
    , X4a
    , X4b
    , X4c
    , X4d
    , X4e
    , X4f

    --
    , X50
    , X51
    , X52
    , X53
    , X54
    , X55
    , X56
    , X57
    , X58
    , X59
    , X5a
    , X5b
    , X5c
    , X5d
    , X5e
    , X5f

    --
    , X60
    , X61
    , X62
    , X63
    , X64
    , X65
    , X66
    , X67
    , X68
    , X69
    , X6a
    , X6b
    , X6c
    , X6d
    , X6e
    , X6f

    --
    , X70
    , X71
    , X72
    , X73
    , X74
    , X75
    , X76
    , X77
    , X78
    , X79
    , X7a
    , X7b
    , X7c
    , X7d
    , X7e
    , X7f

    {--}
    , X100
    , X101
    , X102
    , X103
    , X104
    , X105
    , X106
    , X107
    , X108
    , X109
    , X10a
    , X10b
    , X10c
    , X10d
    , X10e
    , X10f

    --
    , X110
    , X111
    , X112
    , X113
    , X114
    , X115
    , X116
    , X117
    , X118
    , X119
    , X11a
    , X11b
    , X11c
    , X11d
    , X11e
    , X11f

    --
    , X120
    , X121
    , X122
    , X123
    , X124
    , X125
    , X126
    , X127
    , X128
    , X129
    , X12a
    , X12b
    , X12c
    , X12d
    , X12e
    , X12f

    --
    , X130
    , X131
    , X132
    , X133
    , X134
    , X135
    , X136
    , X137
    , X138
    , X139
    , X13a
    , X13b
    , X13c
    , X13d
    , X13e
    , X13f

    --
    , X140
    , X141
    , X142
    , X143
    , X144
    , X145
    , X146
    , X147
    , X148
    , X149
    , X14a
    , X14b
    , X14c
    , X14d
    , X14e
    , X14f

    --
    , X150
    , X151
    , X152
    , X153
    , X154
    , X155
    , X156
    , X157
    , X158
    , X159
    , X15a
    , X15b
    , X15c
    , X15d
    , X15e
    , X15f

    --
    , X160
    , X161
    , X162
    , X163
    , X164
    , X165
    , X166
    , X167
    , X168
    , X169
    , X16a
    , X16b
    , X16c
    , X16d
    , X16e
    , X16f

    --
    , X170
    , X171
    , X172
    , X173
    , X174
    , X175
    , X176
    , X177
    , X178
    , X179
    , X17a
    , X17b
    , X17c
    , X17d
    , X17e
    , X17f

    --
    , X200
    ]
