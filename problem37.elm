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

factors1 : Int -> List Int
factors1 n =
    let
        aux d n =
            if n == 1 then
                []
            else if n `rem` d == 0 then
                d :: aux d (n // d)
            else
                aux (d + 1) n
    in
        aux 2 n

encode1 : List a -> List (number, a)
encode1 list =
    case list of
        [] -> []
        [ x ] -> [ (1, x) ]
        x :: xs ->
            case encode1 xs of
                [] -> []
                (n, y) :: ys ->
                    if x == y then
                        (n+1, x)::ys
                    else
                        (1, x)::(n, y)::ys

swap : (Int, Int) -> (Int, Int)
swap (x,y) = (y, x)

primeFactorsMult1 : Int -> List (Int, Int)
primeFactorsMult1 n =
    factors1 n
        |> encode1
        |> List.map swap

totient1 : Int -> Int
totient1 x =
    List.product
        <| List.map (\(p, c) -> (p - 1) * p ^ (c - 1))
        <| primeFactorsMult1 x

totient2 : Int -> Int
totient2 x =
    let
        aux acc xs =
            case xs of
                [] -> acc
                ((p, m) :: t) ->
                    aux ((p - 1) * p ^ ( m - 1) * acc) t
    in
        aux 1 (primeFactorsMult1 x)

solutions : List (Int -> Int)
solutions =
    [ totient1
    , totient2
    ]

tests : List (Int, Int, Int -> Int -> Bool )
tests =
    [ ( 10, 4, (==)  )
    , ( 13, 12, (==) )
    ]

test : ( Int, Int, Int -> Int -> Bool ) ->
      (Int -> Int) -> Bool
test ( result, expect, compare ) solution =
    compare (solution result) expect
