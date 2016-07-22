import Html exposing (text)
import Problem54 exposing (..)

main =
    let
        result =
            List.map test tests `andMap` solutions
                |> toString
    in
        text result

andMap : List (a -> b) -> List a -> List b
andMap funcs vals =
    List.concatMap (\f -> List.map f vals) funcs

countLeaves1 : Tree a -> List a
countLeaves1 n =
    case n of
        Empty -> []
        Branch x Empty Empty -> [x]
        Branch _ l r ->
            countLeaves1 l ++ countLeaves1 r

countLeaves2 : Tree a -> List a
countLeaves2 n =
    let
        aux x acc =
            case x of
                Empty -> acc
                Branch x Empty Empty -> x::acc
                Branch x l r ->
                    aux l <| aux r acc
    in
        aux n []

solutions : List (Tree a -> List a)
solutions =
    [ countLeaves1
    , countLeaves2
    ]

tests : List ( Tree Int, List Int, List Int -> List Int -> Bool )
tests =
    [ ( Branch 1 ( Branch 1 Empty Empty)
                      ( Branch 1 Empty
                          ( Branch 1 Empty Empty)), [1, 1], (==))
    , ( Branch 1 Empty Empty, [1], (==) )
    , ( Empty, [], (==) )
    ]

test : ( Tree Int, List Int, List Int -> List Int -> Bool ) ->
    ( Tree Int -> List Int ) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
