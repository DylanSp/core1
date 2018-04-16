module Syntax where

type Identifier = String

data Literal = LInt Integer
             | LBool Bool
             deriving (Show, Eq)

data CoreExpr = Var Identifier
              | Lit Literal
              | Apply CoreExpr CoreExpr --function, then argument
              | Lambda Identifier CoreExpr --name of argument, then body 
              deriving (Show, Eq)