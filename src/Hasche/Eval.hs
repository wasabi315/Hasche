{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasche.Eval
  ( MonadEval,
    EvalM,
    runEvalM,
    Error (..),
    eval,
    evalMany,
  )
where

import Control.Exception.Safe
import Control.Monad.Cont
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Hasche.Object
import Hasche.Reader
import Hasche.SExpr
import Prelude hiding (lookup)

-- Monad Stack

type MonadEval m =
  ( MonadCont m,
    MonadIO m,
    MonadThrow m
  )

type EvalM r = ContT r IO

runEvalM :: EvalM r a -> (a -> IO r) -> IO r
runEvalM = runContT

-- Errors

data Error
  = ReadError ReadError
  | SynError Text
  | EvalError Text
  deriving (Show)

instance Exception Error where
  displayException (ReadError err) = "READ ERROR: " ++ displayException err
  displayException (SynError err) = "SYNTAX ERROR: " ++ T.unpack err
  displayException (EvalError err) = "EVAL ERROR: " ++ T.unpack err

-- Evaluation

evalMany :: MonadEval m => Env m -> [SExpr] -> m (ObjRef m)
evalMany env es = do
  os <- traverse (eval env) es
  pure $! maybe undef NE.last (NE.nonEmpty os)

eval :: MonadEval m => Env m -> SExpr -> m (ObjRef m)
eval _ (SList [] Nothing) = pure empty
eval _ (SBool b) = pure if b then true else false
eval _ (SNum n) = num n
eval _ (SStr s) = str s
eval env (SSym s) =
  lookup env s >>= \case
    Nothing -> throw (EvalError $! "Unbound identifier: " <> s)
    Just ref -> pure ref
eval env (SList (x : xs) Nothing) = do
  obj <- eval env x >>= deref
  case obj of
    Syn f -> f env xs
    Prim f -> do
      args <- traverse (eval env) xs
      f env args
    Func env' f -> do
      args <- traverse (eval env) xs
      f env' args
    Cont k -> do
      traverse (eval env) xs >>= \case
        [arg] -> k arg
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")
eval _ (SList _ _) = throw (SynError "proper list required")
