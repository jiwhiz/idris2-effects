module Main

import Effects
import Effect.State

data Tree a
    = Leaf 
    | Node (Tree a) a (Tree a)

flattenTree : Tree a -> List a
flattenTree Leaf = []
flattenTree (Node l x r) = flattenTree l ++ (x :: flattenTree r)

testTree : Tree String
testTree = Node (Node Leaf "One" (Node Leaf "Two" Leaf))
                "Three"
                (Node (Node Leaf "Four" Leaf) "Five" Leaf)

data Tag : Type where
data Leaves : Type where

label : Tree a -> Eff (Tree (Int, a)) [STATE Int]
label Leaf = pure Leaf
label (Node l x r) =
    do
        l' <- label l 
        lbl <- get
        put (lbl + 1)
        r' <- label r
        pure (Node l' (lbl, x) r')

main : IO ()
main = 
    print (flattenTree $ runPureInit [1] (label testTree))
