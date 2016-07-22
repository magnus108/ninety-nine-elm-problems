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

atlevel1 : Int -> Tree a -> List a
atlevel1 n tree =
    case tree of
        Empty -> []
        Branch x l r ->
            if n == 1 then
                [x]
            else if n > 1 then
                atlevel1 (n - 1) l ++ atlevel1 (n - 1) r
            else
                []

solutions : List ((Int, Tree a) -> List a)
solutions =
    [ uncurry atlevel1
    ]

tests : List ( (Int, Tree Int), List Int, List Int -> List Int -> Bool )
tests =
    [ ( (2, Branch 1 ( Branch 1 Empty Empty)
                      ( Branch 1 Empty
                          ( Branch 1 Empty Empty))), [1,1], (==))
    , ( (1, Branch 1 Empty Empty), [1], (==) )
    , ( (0, Empty), [], (==) )
    ]

test : ( (Int, Tree Int), List Int, List Int -> List Int -> Bool ) ->
    ( (Int, Tree Int) -> List Int ) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
