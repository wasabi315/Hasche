{-# LANGUAGE BlockArguments #-}

module Language.Hasche.Evaluate
  ( evalMany,
    eval,
    apply,
  )
where

import Control.Exception.Safe
import Data.Foldable.Extra
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Hasche.Types

newtype EvalError = EvalError T.Text deriving (Show)

instance Exception EvalError

evalMany :: Env -> [Object] -> Eval Object
evalMany env = traverseAndLast (eval env) undef

eval :: Env -> Object -> Eval Object
eval env (Sym s) = do
  res <- findObj env s
  maybe (throw (EvalError $ "Unbound identifier: " <> s)) pure res
eval env o@(Cons {}) = do
  o' NE.:| os <- maybe (throw (EvalError "Proper list required")) pure =<< listify o
  eval env o' >>= \case
    Syn f -> f env os
    Func f -> f =<< traverse (eval env) os
    Cont k ->
      case os of
        [y] -> k =<< eval env y
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")
eval _ o = pure o

-- Procedure application

apply :: Object -> [Object] -> Eval Object
apply x xs =
  case x of
    Func f -> f xs
    Cont k ->
      case xs of
        [arg] -> k arg
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")
