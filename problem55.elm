import Html exposing (text)
import Problem54 exposing (..)

-- this file is bugged

main =
    let
        result = cbalTree 2 |> toString
    in
        text result

cbalTree : Int -> Tree Int
cbalTree n =
    case n of
        0 -> Empty
        1 -> leaf 1
        n ->
            if n % 2 == 1 then
                Branch 1 (cbalTree ((n - 1) // 2)) (cbalTree ((n - 1) // 2))
            else
                Branch 1 (cbalTree ((n - 1) // 2)) (cbalTree (n // 2))

leaf : a -> Tree a
leaf x = Branch x Empty Empty

