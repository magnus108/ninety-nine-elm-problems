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

hbalTree1 : Int -> List (Tree Int)
hbalTree1 n =
    case n of
        0 -> [Empty]
        1 -> [leaf 1]
        x ->
            [(x-2, x-1), (x-1, x-1), (x-1,x-2)]
                `andThen` (\a -> hbalTree1 (fst a)
                `andThen` (\b -> hbalTree1 (snd a)
                `andThen` (\c -> [Branch 1 b c])))

andThen : List a -> (a -> List b) -> List b
andThen = flip List.concatMap

leaf : a -> Tree a
leaf x = Branch x Empty Empty

solutions : List (Int -> List (Tree Int))
solutions =
    [ hbalTree1
    ]

tests : List ( Int, List (Tree Int), List (Tree Int) ->
        List (Tree Int) -> Bool )
tests =
    [ (3, [Branch 1 (Branch 1 Empty Empty)
        (Branch 1 Empty (Branch 1 Empty Empty))
        ,Branch 1 (Branch 1 Empty Empty) (Branch 1 (Branch 1 Empty Empty)
        (Branch 1 Empty Empty))
        ,Branch 1 (Branch 1 Empty Empty) (Branch 1 (Branch 1 Empty Empty)
        Empty),
        Branch 1 (Branch 1 Empty (Branch 1 Empty Empty))
            (Branch 1 Empty (Branch 1 Empty Empty))
        ,Branch 1 (Branch 1 Empty (Branch 1 Empty Empty))
        (Branch 1 (Branch 1 Empty Empty) (Branch 1 Empty Empty))
        ,Branch 1 (Branch 1 Empty (Branch 1 Empty Empty))
        (Branch 1 (Branch 1 Empty Empty) Empty)
        ,Branch 1 (Branch 1 (Branch 1 Empty Empty)
        (Branch 1 Empty Empty))
        (Branch 1 Empty (Branch 1 Empty Empty))
        ,Branch 1 (Branch 1 (Branch 1 Empty Empty)
        (Branch 1 Empty Empty)) (Branch 1 (Branch 1 Empty Empty)
        (Branch 1 Empty Empty)),Branch 1 (Branch 1 (Branch 1 Empty Empty)
        (Branch 1 Empty Empty)) (Branch 1 (Branch 1 Empty Empty) Empty)
        ,Branch 1 (Branch 1 (Branch 1 Empty Empty) Empty)
        (Branch 1 Empty (Branch 1 Empty Empty))
        ,Branch 1 (Branch 1 (Branch 1 Empty Empty) Empty)
        (Branch 1 (Branch 1 Empty Empty) (Branch 1 Empty Empty))
        ,Branch 1 (Branch 1 (Branch 1 Empty Empty) Empty)
        (Branch 1 (Branch 1 Empty Empty) Empty)
        ,Branch 1 (Branch 1 Empty (Branch 1 Empty Empty))
        (Branch 1 Empty Empty),Branch 1 (Branch 1 (Branch 1 Empty Empty)
        (Branch 1 Empty Empty)) (Branch 1 Empty Empty)
        ,Branch 1 (Branch 1 (Branch 1 Empty Empty) Empty)
        (Branch 1 Empty Empty)], (==))
    ]

test : ( Int, List (Tree Int), List (Tree Int) -> List (Tree Int) -> Bool) ->
    ( Int -> List (Tree Int)) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
