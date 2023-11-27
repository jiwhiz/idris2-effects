module Main

import Effects
import Effect.State
import Effect.Exception
import Effect.Random
import Effect.StdIO

data Expr = Var String
          | Val Integer
          | Add Expr Expr
          | Random Integer

Env : Type
Env = List (String, Integer)

getRnd : Integer -> Eff Integer [RND]
getRnd upper = rndInt 0 upper

eval : Expr -> Eff Integer [STDIO, EXCEPTION String, STATE Env, RND]
eval (Var x) =
    case lookup x !(lift get) of
        Nothing => lift $ raise ("No such variable " ++ x)
        Just val => pure val
eval (Val x) = pure x
eval (Add l r) = pure (!(eval l) + !(eval r))
eval (Random x) =
    do
        val <- lift $ getRnd x
        lift $ putStrLn (show val)
        pure val

testExpr : Expr
testExpr = Add (Add (Var "foo") (Val 42)) (Random 100)

runEval : List (String, Integer) -> Expr -> IO Integer
runEval args expr = runInit [(), (), args, 1234] (eval expr)

main : IO ()
main =
    do
        putStr "Number: "
        x <- getLine
        val <- runEval [("foo", cast x)] testExpr
        putStrLn $ "Answer: " ++ show val
        val <- runEval [("foo", cast x)] testExpr
        putStrLn $ "Answer: " ++ show val
