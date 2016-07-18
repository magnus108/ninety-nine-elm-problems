module Problem54 exposing (..)

type Tree a
    = Empty
    | Branch a (Tree a) (Tree a)
