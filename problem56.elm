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

symmetric1 : Tree a -> Bool
symmetric1 t = mirror t t

mirror : Tree a -> Tree a -> Bool
mirror x y =
    case (x, y) of
        (Empty, Empty) -> True
        ((Branch _ l1 r1), (Branch _ l2 r2)) ->
            mirror l1 r2 && mirror r1 l2
        _ -> False

solutions : List (Tree a -> Bool)
solutions =
    [ symmetric1
    ]

-- should there be maybe result?
-- comparer function could be "or"?

tests : List ( Tree Int, Bool, Bool -> Bool -> Bool )
tests =
    [ (Empty, True, (==))
    , (Branch 1 (Branch 1 Empty Empty) (Branch 1 Empty Empty), True, (==))
    , (Branch 1 (Branch 1 Empty (Branch 1 Empty Empty)) Empty, False, (==))
    ]

test : ( Tree a, Bool, Bool -> Bool -> Bool ) ->
    ( Tree a -> Bool) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
