module Problem70 exposing (..)

type Tree a = Node a (List (Tree a))
