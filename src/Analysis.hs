{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Analysis where

import Data.Hashable
import Data.MapLike
import qualified Data.HashMap.Strict as H
import Control.Monad.Trans.Memo.State
import Control.Monad.Memo.Class
import Control.Monad.Memo
import Control.Monad.Identity

import Syntax

countNumericLiterals :: Stmt -> Int
countNumericLiterals p =
  case p of
    Assignment _ e ->
      countNumericLiteralsExpr e
    If c t ->
      countNumericLiteralsExpr c
      + countNumericLiterals t
    IfElse c t e ->
      countNumericLiteralsExpr c
      + countNumericLiterals t
      + countNumericLiterals e
    Block bs ->
      sum (map countNumericLiterals bs)
    While c b ->
      countNumericLiteralsExpr c
      + countNumericLiterals b

countNumericLiteralsExpr :: Expr -> Int
countNumericLiteralsExpr expr =
  case expr of
    Num _ -> 1
    Var _ -> 0
    Neg e -> countNumericLiteralsExpr e
    Add e1 e2 ->
      countNumericLiteralsExpr e1
      + countNumericLiteralsExpr e2
    Sub e1 e2 ->
      countNumericLiteralsExpr e1
      + countNumericLiteralsExpr e2
    Mult e1 e2 ->
      countNumericLiteralsExpr e1
      + countNumericLiteralsExpr e2
    Div e1 e2 ->
      countNumericLiteralsExpr e1
      + countNumericLiteralsExpr e2

-- Memo-cache for 'fm'
type MemoStmt = MemoT Stmt Int
-- Memo-cache for 'gm'
type MemoExpr = MemoT Expr Int

-- | Combined stack of caches (transformers)
-- Stacks two 'MemoT' transformers in one monad to be used in both 'gm' and 'fm' monadic functions
type MemoStmtExpr = MemoStmt (MemoExpr Identity)

-- mCountNumericLiterals :: (MonadMemo Stmt Int m) => Prog -> m Int
mCountNumericLiterals p =
  case p of
    Assignment _ e ->
      memol1 mCountNumericLiteralsExpr e
    If c t -> do
      nc <- memol1 mCountNumericLiteralsExpr c
      nt <- memol0 mCountNumericLiterals t
      pure (nc + nt)
    IfElse c t e -> do
      nc <- memol1 mCountNumericLiteralsExpr c
      nt <- memol0 mCountNumericLiterals t
      ne <- memol0 mCountNumericLiterals e
      pure (nc + nt + ne)
    Block bs -> do
      bs' <- mapM (memol0 mCountNumericLiterals) bs
      pure (sum bs')
    While c b -> do
      nc <- memol1 mCountNumericLiteralsExpr c
      nb <- memol0 mCountNumericLiterals b
      pure (nc + nb)

-- mCountNumericLiteralsExpr :: (MonadMemo Expr Int m) => Expr -> m Int
mCountNumericLiteralsExpr expr =
  case expr of
    Num _ -> pure 1
    Var _ -> pure 0
    Neg e -> memol1 mCountNumericLiteralsExpr e
    Add e1 e2 -> do
      n1 <- memol1 mCountNumericLiteralsExpr e1
      n2 <- memol1 mCountNumericLiteralsExpr e2
      pure (n1 + n2)
    Sub e1 e2 -> do
      n1 <- memol1 mCountNumericLiteralsExpr e1
      n2 <- memol1 mCountNumericLiteralsExpr e2
      pure (n1 + n2)
    Mult e1 e2 -> do
      n1 <- memol1 mCountNumericLiteralsExpr e1
      n2 <- memol1 mCountNumericLiteralsExpr e2
      pure (n1 + n2)
    Div e1 e2 -> do
      n1 <- memol1 mCountNumericLiteralsExpr e1
      n2 <- memol1 mCountNumericLiteralsExpr e2
      pure (n1 + n2)

instance (Eq k, Hashable k) => MapLike (H.HashMap k v) k v where
    lookup = H.lookup
    add = H.insert

startEvalMemo' x = x `evalMemoState` H.empty
startRunMemo' x = x `runMemoState` H.empty

startEvalMemoT' x = x `evalMemoStateT` H.empty
startRunMemoT' x = x `evalMemoStateT` H.empty

evalAll = startEvalMemo' . startEvalMemoT'
runAll = startRunMemo' . startRunMemoT'

evalStmt = evalAll . mCountNumericLiterals
evalExpr = evalAll . mCountNumericLiteralsExpr

runStmt = runAll . mCountNumericLiterals
runExpr = runAll . mCountNumericLiteralsExpr
