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

and1' : Bool -> Bool -> Bool
and1' = (&&)

and2' : Bool -> Bool -> Bool
and2' a b =
    case (a, b) of
        (True, True) -> True
        _ -> False

and3' : Bool -> Bool -> Bool
and3' a b = a && b

or1' : Bool -> Bool -> Bool
or1' = (||)

nand1' : Bool -> Bool -> Bool
nand1' = not >> and1'

nor1' : Bool -> Bool -> Bool
nor1' = not >> or1'

xor1' : Bool -> Bool -> Bool
xor1' = not >> equ1'

impl1' : Bool -> Bool -> Bool
impl1' = not >> nor1'

equ1' : Bool -> Bool -> Bool
equ1' = (==)

solutions : List (Int -> List (Tree Int))
solutions =
    [
    ]

tests : List ( Int, List (Tree Int) )
tests =
    [
    ]

test : ( Int, List (Tree Int) ) ->
    ( Int -> List ( Tree Int )) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
