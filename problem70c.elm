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

nnodes1 : Tree Int -> Int
nnodes1 tree =
    let
        (Node x xs) = tree
    in
        1 + (List.sum (List.map nnodes1 xs))

solutions : List (Tree Int -> Int)
solutions =
    [ nnodes1
    ]

tests : List ( Tree Int, Int, Int -> Int -> Bool )
tests =
    [ ( Node 1 [Node 1 [], Node 3 [Node 2 []]], 4, (==) )
    , ( Node 1 [], 1, (==))
    ]

test : ( Tree Int, Int, Int -> Int -> Bool ) ->
    ( Tree Int -> Int) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
