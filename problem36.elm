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

primeFactorsMult2 : Int -> List (Int, Int)
primeFactorsMult2 n =
    let
        aux d n =
            if n == 1 then
                []
            else if n `rem` d == 0 then
                case aux d (n // d) of
                    ((h, n) :: t) ->
                        if h == d then
                            (h, n+1) :: t
                        else
                            (d, 1) :: ((h, n) ::t)
                    l ->
                        (d, 1) :: l
            else
                aux (d + 1) n
    in
        aux 2 n

solutions : List (Int -> List (Int, Int))
solutions =
    [ primeFactorsMult1
    , primeFactorsMult2
    ]

tests : List (Int, List (Int, Int), List (Int, Int) ->
      List (Int, Int) -> Bool )
tests =
    [ ( 315, [(3,2),(5,1),(7,1)], (==)  )
    , ( 2, [(2,1)], (==) )
    ]

test : ( Int, List (Int, Int), List (Int, Int) -> List (Int, Int) -> Bool ) ->
      (Int -> List (Int, Int)) -> Bool
test ( result, expect, compare ) solution =
    compare (solution result) expect
