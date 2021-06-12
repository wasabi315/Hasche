{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.Reader
import Data.Foldable.Extra
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
  ( MonadReader (Env m) m, -- for accessing top-level environment
    MonadCont m, -- for call/cc
    MonadIO m,
    MonadThrow m
  )

newtype EvalM r a = EvalM (ReaderT (Env (EvalM r)) (ContT r IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCont,
      MonadReader (Env (EvalM r))
    )

runEvalM :: EvalM r a -> Env (EvalM r) -> (a -> IO r) -> IO r
runEvalM (EvalM m) = runContT . runReaderT m

-- Errors

data Error
  = ReadError ReadError
  | FileError IOException
  | SynError Text
  | EvalError Text
  | UserError Text
  deriving (Show)

instance Exception Error where
  displayException (ReadError e) = "[READ ERROR]: " ++ displayException e
  displayException (FileError e) = "[FILE ERROR]:" ++ displayException e
  displayException (SynError e) = "[SYNTAX ERROR]: " ++ T.unpack e
  displayException (EvalError e) = "[EVAL ERROR]: " ++ T.unpack e
  displayException (UserError e) = "[USER ERROR]: " ++ T.unpack e

-- Evaluation

evalMany :: MonadEval m => Env m -> [SExpr] -> m (Object m)
evalMany env = traverseAndLast (eval env) undef

eval :: MonadEval m => Env m -> SExpr -> m (Object m)
eval _ SEmpty = pure empty
eval _ (SBool b) = pure if b then true else false
eval _ (SNum n) = num n
eval _ (SStr s) = str s
eval env (SSym s) =
  lookup env s >>= \case
    Nothing -> throw (EvalError $ "Unbound identifier: " <> s)
    Just ref -> deref ref
eval env (SList (x NE.:| xs)) = do
  obj <- eval env x
  case obj of
    Syn f -> f env xs
    Func f -> traverse (eval env) xs >>= f
    Cont k ->
      case xs of
        [y] -> eval env y >>= k
        _ -> throw (EvalError "Arity mismatch")
    _ -> throw (EvalError "Could not apply")
eval _ (SDList _ _) = throw (SynError "proper list required")
