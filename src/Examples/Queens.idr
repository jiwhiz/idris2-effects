module Main

import Effects
import Effect.Select

no_attack : (Int, Int) -> (Int, Int) -> Bool
no_attack (x, y) (x', y') 
   = x /= x' && y /= y' && abs (x - x') /= abs (y - y')

rowsIn : Int -> List (Int, Int) -> List Int
rowsIn col qs = [ x | x <- [1..8], all (no_attack (x, col)) qs ]

addQueens : Int -> List (Int, Int) -> Eff (List (Int, Int)) [SELECT]
addQueens 0   qs = pure qs
addQueens col qs = do row <- select (rowsIn col qs)
                      addQueens (col - 1) ((row, col) :: qs)

getQueens : Maybe (List (Int, Int))
getQueens = runInit [()] (addQueens 8 [])

getAllQueens : List (List (Int, Int))
getAllQueens = runInit [()] (addQueens 8 [])

main : IO ()
main =
    do
        putStrLn ("One Solution:\n" ++ show getQueens)
        putStrLn ("All Solutions:\n" ++ show getAllQueens)
