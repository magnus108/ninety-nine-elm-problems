import Html exposing (text)

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

goldbachList1 : Int -> Int -> List (Int, Maybe (Int, Int))
goldbachList1 a b =
    if a > b then
        []
    else if a `rem` 2 == 1 then
        goldbachList1 (a + 1) b
    else
        (a, goldbach1 a) :: goldbachList1 (a + 2) b

goldbach1 : Int -> Maybe (Int, Int)
goldbach1 a =
    List.head <|
        List.filter(\(x, y) -> isPrime x && isPrime y) <|
            List.map (\e -> (e, a - e)) (primeR 1 (a//2))

primeR : Int -> Int -> List Int
primeR a b =
    List.filter isPrime [ a .. b]

isPrime : Int -> Bool
isPrime n =
    if n < 2 then
        False
    else
        eratos (abs n) [2..(n // 2)]

eratos : Int -> List Int -> Bool
eratos n list =
    case list of
        [] -> True
        x::xs ->
            if n % x == 0 then
                False
            else
                eratos n (List.filter (\y -> (y % x) /= 0) xs)

solutions : List ((Int, Int) -> List (Int, Maybe (Int, Int)))
solutions =
    [ uncurry goldbachList1
    ]

tests : List ( (Int, Int), List (Int, Maybe (Int, Int)),
    List (Int, Maybe (Int, Int)) -> List (Int, Maybe (Int, Int)) -> Bool )
tests =
    [ ((9, 20), [(10,Just (3,7))
                ,(12,Just (5,7))
                ,(14,Just (3,11))
                ,(16,Just (3,13))
                ,(18,Just (5,13))
                ,(20,Just (3,17))], (==))
    ]

test : ((Int, Int), List (Int, Maybe (Int, Int)),
    List (Int, Maybe (Int, Int)) -> List (Int, Maybe (Int, Int)) -> Bool) ->
        ((Int, Int) -> List (Int, Maybe (Int, Int))) -> Bool
test ( result, expect, compare ) solution =
    compare (solution result) expect
