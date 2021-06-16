{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hasche.Eval.Eval
  ( MonadEval,
    eval,
    evalMany,
    apply,
  )
where

import Control.Exception.Safe
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Cell qualified as Cell
import Data.Foldable.Extra
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Language.Hasche.Eval.Env qualified as Env
import Language.Hasche.Eval.Error
import Language.Hasche.Eval.Object
import Language.Hasche.Syntax.SExpr
import Prelude hiding (lookup)

-- Monad Stack

type MonadEval m =
  ( MonadReader (Env m) m, -- for accessing top-level environment
    MonadCont m, -- for call/cc
    MonadIO m,
    MonadThrow m
  )

-- Evaluation

evalMany :: MonadEval m => Env m -> [SExpr] -> m (Object m)
evalMany env = traverseAndLast (eval env) undef

eval :: MonadEval m => Env m -> SExpr -> m (Object m)
eval _ SEmpty = pure empty
eval _ (SBool b) = pure if b then true else false
eval _ (SNum n) = num n
eval _ (SStr s) = str s
eval env (SSym s) =
  Env.lookup env s >>= \case
    Nothing -> throw (EvalError $ "Unbound identifier: " <> s)
    Just ref -> Cell.deref ref
eval env (SList (x NE.:| xs)) =
  eval env x >>= \case
    Syn f -> f env xs
    Func f -> traverse (eval env) xs >>= f
    Cont k ->
      case xs of
        [y] -> eval env y >>= k
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")
eval _ (SDList _ _) = throw (SynError "proper list required")

-- Procedure application

apply :: MonadEval m => Object m -> [Object m] -> m (Object m)
apply x xs =
  case x of
    Func f -> f xs
    Cont k ->
      case xs of
        [arg] -> k arg
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")
