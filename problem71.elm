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

ipl1 : Tree a -> Int
ipl1 tree =
    let
        aux d (Node x ts) =
            d + (List.sum (List.map (aux ( d + 1 )) ts))
    in
        aux 0 tree

solutions : List (Tree a -> Int)
solutions =
    [ ipl1
    ]

tests : List ( Tree Int, Int, Int -> Int -> Bool )
tests =
    [ ( Node 1 [Node 1 [], Node 3 [Node 2 []]], 4, (==) )
    , ( Node 1 [], 0, (==))
    ]

test : ( Tree Int, Int, Int -> Int -> Bool ) ->
    ( Tree Int -> Int) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
