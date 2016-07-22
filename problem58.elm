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

symCbalTrees1 : Int -> List (Tree Int)
symCbalTrees1 = List.filter symmetric1 << cbalTree1

symmetric1 : Tree a -> Bool
symmetric1 t = mirror t t

mirror : Tree a -> Tree a -> Bool
mirror x y =
    case (x, y) of
        (Empty, Empty) -> True
        ((Branch _ l1 r1), (Branch _ l2 r2)) ->
            mirror l1 r2 && mirror r1 l2
        _ -> False

cbalTree1 : Int -> List (Tree Int)
cbalTree1 n =
    case n of
        0 -> [Empty]
        1 -> [leaf 1]
        n ->
            let
                x = cbalTree1 ((n - 1) // 2)
            in
                if n % 2 == 1 then
                    lift2 (Branch 1) x x
                else
                    let
                        y = cbalTree1 (n // 2)
                    in
                        lift2 (Branch 1) x y ++ lift2 (Branch 1) y x

quotRem : Int -> Int -> ( Int, Int )
quotRem x y =
    ( x // y, x `rem` y )

andThen : List a -> (a -> List b) -> List b
andThen = flip List.concatMap

lift2 : (a -> b -> c) -> List a -> List b -> List c
lift2 f la lb =
    la `andThen` (\a -> lb `andThen` (\b -> [f a b]))

leaf : a -> Tree a
leaf x = Branch x Empty Empty

solutions : List (Int -> List (Tree Int))
solutions =
    [ symCbalTrees1
    ]

tests : List ( Int, List (Tree Int), List (Tree Int) ->
        List (Tree Int) -> Bool)
tests =
    [ (5, [ Branch 1 (Branch 1 Empty (Branch 1 Empty Empty))
                    (Branch 1 (Branch 1 Empty Empty) Empty)
          , Branch 1 (Branch 1 (Branch 1 Empty Empty) Empty)
                    (Branch 1 Empty (Branch 1 Empty Empty))], (==))
    ]

test : ( Int, List (Tree Int), List (Tree Int) -> List (Tree Int) -> Bool ) ->
    ( Int -> List (Tree Int)) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
