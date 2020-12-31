module Placeholders exposing (main)

import Browser
import Html exposing (Html, br, div, text)


{-| doAdd : number -> number -> number
-}
doAdd a b =
    let
        placeholder0 =
            (+)

        placeholder1 =
            a

        placeholder2 =
            b
    in
    placeholder0 placeholder1 placeholder2


{-| sum : List number -> number
-}
sum list =
    let
        placeholder3 =
            List.foldl

        placeholder4 =
            doAdd

        placeholder5 =
            0

        placeholder6 =
            list
    in
    placeholder3 placeholder4 placeholder5 placeholder6



-- numberTotal : number
-- should be illegal! Can't generate instructions for `number`, only Int or Float


numberTotal =
    let
        placeholder7 =
            sum

        placeholder8 =
            [ 1, 2, 3 ]
    in
    placeholder7 placeholder8



-- supertype specialization: needs to affect code gen somehow!


intList : List Int
intList =
    [ 1, 2, 3 ]



-- intTotal : Int


intTotal =
    let
        placeholder9 =
            sum

        placeholder10 =
            intList
    in
    placeholder9 placeholder10



-- floatTotal : Float


floatTotal =
    let
        placeholder11 =
            sum

        placeholder12 =
            [ 1.1, 2.2, 3.3 ]
    in
    placeholder11 placeholder12


main : Html ()
main =
    div []
        [ text <| "intTotal = " ++ String.fromInt intTotal
        , br [] []
        , text <| "floatTotal = " ++ String.fromFloat floatTotal
        ]
