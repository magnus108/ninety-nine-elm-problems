import Html exposing (text)
import Problem70 exposing (..)

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

bottomUp1 : Tree a -> List a
bottomUp1 tree =
    let
        aux : Tree a -> List a -> List a
        aux (Node x ts) acc =
            List.foldr aux (x::acc) ts
    in
        aux tree []

solutions : List (Tree a -> List a)
solutions =
    [ bottomUp1
    ]

tests : List ( Tree Int, List Int, List Int -> List Int -> Bool )
tests =
    [ ( Node 1 [Node 1 [], Node 3 [Node 2 []]], [1,2,3,1], (==) )
    , ( Node 1 [], [1], (==))
    ]

test : ( Tree Int, List Int, List Int -> List Int -> Bool ) ->
    ( Tree Int -> List Int) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
