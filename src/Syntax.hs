module Syntax where

type Identifier = String

data Literal = LInt Integer
             | LBool Bool
             deriving (Show, Eq)

--division deliberately omitted to avoid dealing with integer division complexity
data BinOp = Add
           | Subtract
           | Multiply
           | Equals
           deriving (Show, Eq)

data CoreExpr = Var Identifier
              | Lit Literal
              | Apply CoreExpr CoreExpr --function, then argument
              | Lambda Identifier Type CoreExpr --name of argument, type of argument, then body
              | Let Identifier CoreExpr CoreExpr --let a b e is equivalent to let a = b in e
              | If CoreExpr CoreExpr CoreExpr --if cond a b is equivalent to if cond then a else b
              | Op BinOp CoreExpr CoreExpr
              | Fix CoreExpr
              deriving (Show, Eq)

type Decl = (String, CoreExpr) -- top-level let declaration

data Program = Program [Decl] CoreExpr -- last CoreExpr is the main function

data Type = TVariable String
          | TConstructor String
          | TFunction Type Type
          deriving (Show, Eq)

tInt, tBool :: Type
tInt = TConstructor "Int"
tBool = TConstructor "Bool"