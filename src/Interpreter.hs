module Interpreter where

import Syntax

import qualified Data.Map as Map

type Env = Map.Map String Int

-- note that variables are not lexically scoped inside blocks
exec :: Stmt -> Env -> Env
exec stmt env =
  case stmt of
    Assignment var expr ->
      let result = eval expr env
      in update var result env
    If cond conseq ->
      let test = eval cond env
      in if test /= 0
         then exec conseq env
         else env
    IfElse cond conseq alt -> do
      let test = eval cond env
      if test /= 0
         then exec conseq env
         else exec alt env
    Block (b:bs) -> do
      let env' = exec b env
      exec (Block bs) env'
    Block [] -> env
    While cond body -> do
      let test = eval cond env
      if test /= 0
         then exec (Block [body, While cond body]) env
         else env

eval :: Expr -> Env -> Int
eval expr env =
  case expr of
    Num x -> x
    Var v ->
      case Map.lookup v env of
        Nothing -> error "unknown variable"
        Just x -> x
    Neg e ->
      let x = eval e env
      in negate x
    _     ->
      let x = eval (e1 expr) env
          y = eval (e2 expr) env
      in getOp expr x y

update :: String -> Int -> Env -> Env
update v x env = case Map.lookup v env of
                   Nothing -> Map.insert v x env
                   Just _ -> Map.update f v env
  where f _ = Just x

getOp :: Expr -> (Int -> Int -> Int)
getOp (Add _ _) = (+)
getOp (Sub _ _) = (-)
getOp (Mult _ _) = (*)
getOp (Div _ _) = div
