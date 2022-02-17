{-# LANGUAGE DeriveGeneric #-}

module Syntax where

import Data.Hashable
import Data.Hashable.Lifted
import GHC.Generics (Generic)

type Prog = Stmt

data Expr = Num Int
          | Var String
          | Neg Expr
          | Add {e1 :: Expr, e2 :: Expr}
          | Sub {e1 :: Expr, e2 :: Expr}
          | Mult {e1 :: Expr, e2 :: Expr}
          | Div {e1 :: Expr, e2 :: Expr}
          deriving (Eq, Show, Generic)

instance Hashable Expr

data Stmt = Assignment String Expr
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | Block [Stmt]
          | While Expr Stmt
          deriving (Eq, Show, Generic)

instance Hashable Stmt
