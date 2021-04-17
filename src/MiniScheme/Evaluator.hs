{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Evaluator
  ( evaluate,
    Env,
    Value,
    EvalError,
  )
where

import Control.Exception.Safe
import Data.Maybe
import MiniScheme.AST qualified as AST
import MiniScheme.Evaluator.Builtins
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Eval
import MiniScheme.Evaluator.Monad
import Prelude hiding (lookup)

newtype Value = Value (Value' Evaluator)
  deriving newtype (Show)

newtype Env = Env (Env' Evaluator)

evaluate :: Maybe Env -> AST.Prog -> IO (Either EvalError (Value, Env))
evaluate menv prog =
  catch
    do
      env <- maybe builtinEnv (\(Env e) -> pure e) menv
      v <- runEvaluator (eval env prog)
      pure $ Right (Value v, Env env)
    (pure . Left)
