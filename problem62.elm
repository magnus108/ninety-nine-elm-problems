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

internals1 : Tree a -> List a
internals1 n =
    case n of
        Empty -> []
        Branch _ Empty Empty -> []
        Branch x l r ->
            x::internals1 l ++ internals1 r

internals2 : Tree a -> List a
internals2 n =
    let
        aux x acc =
            case x of
                Empty -> acc
                Branch _ Empty Empty -> acc
                Branch x l r ->
                    x::(aux l <| aux r acc)
    in
        aux n []

solutions : List (Tree a -> List a)
solutions =
    [ internals1
    , internals2
    ]

tests : List ( Tree Int, List Int, List Int -> List Int -> Bool )
tests =
    [ ( Branch 1 ( Branch 1 Empty Empty)
                      ( Branch 1 Empty
                          ( Branch 1 Empty Empty)), [1,1], (==))
    , ( Branch 1 Empty Empty, [], (==) )
    , ( Empty, [], (==) )
    ]

test : ( Tree Int, List Int, List Int -> List Int -> Bool ) ->
    ( Tree Int -> List Int ) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
