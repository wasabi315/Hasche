{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Interpreter
  ( interpret,
    Env,
    Value,
    EvalError,
  )
where

import Control.Exception.Safe
import Data.Maybe
import MiniScheme.AST qualified as AST
import MiniScheme.Interpreter.Builtins
import MiniScheme.Interpreter.Data
import MiniScheme.Interpreter.Interpreter
import MiniScheme.Interpreter.Monad
import Prelude hiding (lookup)

newtype Value = Value (Value' Interpreter)
  deriving newtype (Show)

newtype Env = Env (Env' Interpreter)

interpret :: Maybe Env -> AST.Prog -> IO (Either EvalError (Value, Env))
interpret menv prog =
  catch
    do
      env <- maybe builtinEnv (\(Env e) -> pure e) menv
      v <- runInterpreter (eval env prog)
      pure $ Right (Value v, Env env)
    (pure . Left)
