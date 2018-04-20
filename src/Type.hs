{-# LANGUAGE LambdaCase #-}

module Type where

-- need the MTL versions of these libraries for auto-lifting
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map

import Syntax

data TypeError = Mismatch Type Type
               | NotFunction Type -- attempting to apply a non-function to an argument, or fix a non-function
               | NotInScope Identifier
               deriving (Show, Eq)

type TypingEnv = Map.Map Identifier Type

-- monad stack to store typing environment, typing errors
type Check a = ExceptT TypeError (Reader TypingEnv) a

extend :: Identifier -> Type -> TypingEnv -> TypingEnv
extend name typ env = Map.insert name typ env

-- add name: typ to the environment, then run a typecheck
checkInEnv :: Identifier -> Type -> Check a -> Check a
checkInEnv name typ = local $ extend name typ

lookupVar :: Identifier -> Check Type
lookupVar name = do
    env <- ask
    case Map.lookup name env of
        Just t -> return t
        Nothing -> throwError $ NotInScope name

typeCheck :: CoreExpr -> Check Type
typeCheck = \case
    Lit (LInt _) -> return TInt

    Lit (LBool _) -> return TBool

    Var name -> lookupVar name

    Lambda name typ body -> do
        rhs <- checkInEnv name typ (typeCheck body)
        return $ TFunction typ rhs

    Apply func arg -> do
        funcType <- typeCheck func
        argType <- typeCheck arg
        case funcType of
            TFunction a b | a == argType -> return b
                          | otherwise -> throwError $ Mismatch argType a
            nonFunc -> throwError $ NotFunction nonFunc

    Let name value body -> undefined

    If cond tr fl -> do
        condType <- typeCheck cond
        trueType <- typeCheck tr
        falseType <- typeCheck fl
        if condType /= TBool
            then throwError $ Mismatch condType TBool
            else if trueType /= falseType
                then throwError $ Mismatch trueType falseType
                else return trueType

    Op op left right -> do
        leftType <- typeCheck left
        rightType <- typeCheck right
        case op of
            -- only define equality on ints
            Equals -> case (leftType, rightType) of
                        (TInt, TInt) -> return TBool
                        (TInt, bad) -> throwError $ Mismatch TInt bad
                        (bad, _) -> throwError $ Mismatch bad TInt
            -- Add, Subtract, Multiply
            _ -> case (leftType, rightType) of
                    (TInt, TInt) -> return TInt
                    (TInt, bad) -> throwError $ Mismatch TInt bad
                    (bad, _) -> throwError $ Mismatch bad TInt

    Fix expr -> do
        innerType <- typeCheck expr
        case innerType of
                    TFunction a b | a == b -> return a
                                  | otherwise -> throwError $ Mismatch a b
                    nonFunc -> throwError $ NotFunction nonFunc

runTypecheck :: TypingEnv -> Check a -> Either TypeError a
runTypecheck env checker = runReader (runExceptT checker) env