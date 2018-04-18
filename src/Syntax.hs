module Syntax where

type Identifier = String

data Literal = LInt Integer
             | LBool Bool
             deriving (Show, Eq)

data CoreExpr = Var Identifier
              | Lit Literal
              | Apply CoreExpr CoreExpr --function, then argument
              | Lambda Identifier CoreExpr --name of argument, then body
              | Let Identifier CoreExpr CoreExpr --let a b e is equivalent to let a = b in e
              | If CoreExpr CoreExpr CoreExpr --if cond a b is equivalent to if cond then a else b
              deriving (Show, Eq)