{-# LANGUAGE LambdaCase #-}

module Eval (
    eval
) where

import Syntax

import qualified Data.Map as Map

type Environment = Map.Map String Value

data Value = VInt Integer
           | VBool Bool
           | VClosure String CoreExpr Environment
           deriving Eq

instance Show Value where
    show (VInt n) = show n
    show (VBool b) = show b
    show (VClosure _ _ _) = "<closure>"

eval :: Environment -> CoreExpr -> Value
eval env = \case
    Lit (LInt n) -> VInt n
    Lit (LBool b) -> VBool b
    Var name -> case (Map.lookup name env) of
        (Just val) -> val
        Nothing -> error $ "Variable " ++ show name ++ " not in scope"
    Lambda name body -> VClosure name body env
    Apply func arg -> apply (eval env func) (eval env arg)

--beta reduction: replace a bound variable in the lambda with the argument to the lambda
apply :: Value -> Value -> Value
apply (VClosure name body env) arg = eval (Map.insert name arg env) body
apply _ _ = error "Tried to apply a non-closure"
