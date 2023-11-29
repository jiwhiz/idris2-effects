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
data Counts : Type where

label : Tree a -> Eff (Tree (Int, a)) [Counts ## STATE Int, Tag ## STATE Int] 
label Leaf =
    do
        lift $ Counts #- update (+ (cast {to=Int} 1))
        pure Leaf
label (Node l x r) =
    do
        l' <- label l 
        lbl <- lift $ Tag #- get
        lift $ Tag #- put (lbl + 1)
        r' <- label r
        pure (Node l' (lbl, x) r')

partial
main : IO ()
main = 
    do
        let (tree ** [Counts #= l, _]) := runPureEnv [Counts #= 0, Tag #= 1] (label testTree)
        printLn (flattenTree tree, l)
