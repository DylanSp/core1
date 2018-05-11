{-# LANGUAGE LambdaCase #-}

module Type (
    TypeError(..)
  , typeOf
) where

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

type TypingSubstitution = Map.Map TypeVariable Type

-- monad stack to store typing environment, typing errors
type Check a = ExceptT TypeError (Reader (TypingEnv, TypingSubstitution)) a

extendEnv :: Identifier -> Type -> TypingEnv -> TypingEnv
extendEnv = Map.insert

extendSub :: TypeVariable -> Type -> TypingSubstitution -> TypingSubstitution
extendSub = Map.insert

-- add name: typ to the environment, then run a typecheck
checkInEnv :: Identifier -> Type -> Check a -> Check a
checkInEnv name typ = local $ \(env, sub) -> (extendEnv name typ env, sub)

lookupVar :: Identifier -> Check Type
lookupVar name = do
    (env, _) <- ask
    case Map.lookup name env of
        Just t -> return t
        Nothing -> throwError $ NotInScope name

typeCheck :: CoreExpr -> Check Type
typeCheck = \case
    Lit (LInt _) -> return tInt

    Lit (LBool _) -> return tBool

    Var name -> lookupVar name

    Lambda name typ body -> do
        rhs <- checkInEnv name typ (typeCheck body)
        return $ TFunction typ rhs

    Apply func arg -> do
        funcType <- typeCheck func
        argType <- typeCheck arg
        (_, subst) <- ask

        case funcType of
            TFunction a b -> case a of
                TVariable tvar -> applySubstitution (extendSub tvar argType subst) b
                -- TConstructor, TFunction
                _ -> if a == argType
                        then return b
                        else throwError $ Mismatch argType a
            nonFunc -> throwError $ NotFunction nonFunc

    Let name value body -> do
        valueType <- typeCheck value
        checkInEnv name valueType (typeCheck body)

    If cond tr fl -> do
        condType <- typeCheck cond
        trueType <- typeCheck tr
        falseType <- typeCheck fl
        if condType /= tBool
            then throwError $ Mismatch condType tBool
            else if trueType /= falseType
                then throwError $ Mismatch trueType falseType
                else return trueType

    Op op left right -> do
        leftType <- typeCheck left
        rightType <- typeCheck right
        case op of
            -- only define equality on ints
            Equals -> if leftType == tInt 
                        then if rightType == tInt
                                then return tBool
                                else throwError $ Mismatch tInt rightType
                        else throwError $ Mismatch leftType tInt
            -- Add, Subtract, Multiply
            _ -> if leftType == tInt 
                    then if rightType == tInt
                        then return tInt
                        else throwError $ Mismatch tInt rightType
                    else throwError $ Mismatch leftType tInt

    Fix expr -> do
        innerType <- typeCheck expr
        case innerType of
                    TFunction a b | a == b -> return a
                                  | otherwise -> throwError $ Mismatch a b
                    nonFunc -> throwError $ NotFunction nonFunc

-- make this a typeclass method later, when it can be applied to other type-containing structures?
-- make it TypingSubstitution -> Type -> Type? but then how to handle out-of-scope errors?
applySubstitution :: TypingSubstitution -> Type -> Check Type
applySubstitution subst typ = case typ of
    TVariable tvar -> case Map.lookup tvar subst of
        Just typ' -> return typ'
        Nothing -> return typ
    TConstructor _ -> return typ
    TFunction tArg tBody -> TFunction <$> (applySubstitution subst tArg) <*> (applySubstitution subst tBody)

runTypecheck :: (TypingEnv, TypingSubstitution) -> Check a -> Either TypeError a
runTypecheck (env, sub) checker = runReader (runExceptT checker) (env, sub)

typeOf :: CoreExpr -> Either TypeError Type
typeOf expr = runTypecheck (Map.empty, Map.empty) (typeCheck expr)