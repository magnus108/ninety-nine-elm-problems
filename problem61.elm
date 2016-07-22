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

countLeaves1 : Tree a -> Int
countLeaves1 n =
    case n of
        Empty -> 0
        Branch _ Empty Empty -> 1
        Branch _ l r ->
            countLeaves1 l + countLeaves1 r

solutions : List (Tree a -> Int)
solutions =
    [ countLeaves1
    ]

tests : List ( Tree Int, Int, Int -> Int -> Bool )
tests =
    [ ( Branch 1 ( Branch 1 Empty Empty)
                      ( Branch 1 Empty
                          ( Branch 1 Empty Empty)), 2, (==))
    , ( Branch 1 Empty Empty, 1, (==) )
    , ( Empty, 0, (==) )
    ]

test : ( Tree Int, Int, Int -> Int -> Bool ) ->
    ( Tree Int -> Int ) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
