module Main where

import Lib

main :: IO ()
main = someFunc

data Expr = Add Expr Expr
          | Mult Expr Expr
          | Num Int
          deriving (Eq, Show)

eval :: Expr -> Int
eval (Num x) = x
eval (Add e e')  = eval e + eval e'
eval (Mult e e') = eval e * eval e'

data ExprC = AddLeftC   Expr
           | AddRightC  Expr
           | MultLeftC  Expr
           | MultRightC Expr
           deriving (Eq, Show)

type ExprZipper = (Expr, [ExprC])

mkZipper :: Expr -> ExprZipper
mkZipper e = (e, [])

goUp :: ExprZipper -> ExprZipper
goUp (e, (AddLeftC e' : cs))   = (Add e e', cs)
goUp (e, (AddRightC e' : cs))  = (Add e' e, cs)
goUp (e, (MultLeftC e' : cs))  = (Mult e e', cs)
goUp (e, (MultRightC e' : cs)) = (Mult e' e, cs)

goLeft :: ExprZipper -> ExprZipper
goLeft (Add e e', cs)  = (e, AddLeftC e' : cs)
goLeft (Mult e e', cs) = (e, MultLeftC e' : cs)

goRight :: ExprZipper -> ExprZipper
goRight (Add e e', cs)  = (e', AddRightC e : cs)
goRight (Mult e e', cs) = (e', MultRightC e : cs)


x -: f = f x

myExpr = Add (Num 1) (Mult (Num 2) (Num 3))

eval' :: ExprZipper -> [(ExprZipper, Int)]
eval' z@(Num x, cs) = [(z, x)]
eval' z@(e, cs) = (z, x) : (c1 ++ c2)
  where c1@((e1, v1):_) = eval' $ goLeft z
        c2@((e2, v2):_) = eval' $ goRight z
        x = v1 `op` v2
        op = case e of
          (Add _ _)  -> (+)
          (Mult _ _) -> (*)

printList :: (Show a) => [a] -> IO ()
printList = mapM_ print
