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

cbalTree2 : Int -> List (Tree Int)
cbalTree2 n =
    case n of
        0 -> [Empty]
        n ->
            let
                (p, q) = (n - 1) `quotRem` 2
                lr = List.map (\ i ->
                    (cbalTree2 i, cbalTree2 ( n - i - 1 ))) [p .. p + q]
            in
                List.concatMap (uncurry (lift2 (Branch 1))) lr

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
    [ cbalTree1
    , cbalTree2
    ]

tests : List ( Int, List (Tree Int) )
tests =
    [ ( 4, [ Branch 1 ( Branch 1 Empty Empty)
                      ( Branch 1 Empty
                          ( Branch 1 Empty Empty))
           , Branch 1 ( Branch 1 Empty Empty)
                      ( Branch 1 (Branch 1 Empty Empty)
                          Empty)
           , Branch 1 ( Branch 1 Empty
                          ( Branch 1 Empty Empty))
                      ( Branch 1 Empty Empty)
           , Branch 1 ( Branch 1 (Branch 1 Empty Empty)
                           Empty)
                      ( Branch 1 Empty Empty)])
    , ( 1, [ leaf 1 ] )
    , ( 0, [ Empty ] )
    ]

test : ( Int, List (Tree Int) ) ->
    ( Int -> List ( Tree Int )) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
