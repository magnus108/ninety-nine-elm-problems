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

gray1 : Int -> List String
gray1 n =
    case n of
        0 -> [""]
        n ->
            List.foldr (\s acc ->
                ("0" ++ s)::("1" ++ s)::acc) [] (gray1 (n - 1))

gray2 : Int -> List String
gray2 n =
    case n of
        0 -> [""]
        x ->
            let
                xs = gray2 (n - 1)
            in
                List.map ((++)"0") xs ++ List.map ((++)"1") (List.reverse xs)

isPermutationOf : List a -> List a -> Bool
isPermutationOf permut xs = permut `List.member` permutations xs

permutations : List a -> List (List a)
permutations xs' =
  case xs' of
    [] -> [[]]
    xs ->
        let
            f (y,ys) = List.map ((::)y) (permutations ys)
        in
            List.concatMap f (select xs)

select : List a -> List (a, List a)
select xs =
  case xs of
    [] -> []
    (x::xs) -> (x,xs)::List.map (\(y,ys) -> (y,x::ys)) (select xs)

solutions : List (Int -> List String)
solutions =
    [ gray1
    , gray2
    ]

tests : List (Int, List String, List a -> List a -> Bool )
tests =
    [ ( 3, ["000","100","010","110","001","101","011","111"], isPermutationOf )
    , ( 1, ["0", "1"], isPermutationOf )
    , ( 0, [""], isPermutationOf )
    ]

test : ( Int, List String, List String -> List String -> Bool ) ->
      (Int -> List String) -> Bool
test ( result, expect, compare ) solution =
    compare (solution result) expect
