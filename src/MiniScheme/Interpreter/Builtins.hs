{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Interpreter.Builtins
  ( builtinEnv,
  )
where

import Control.Exception.Safe
import Control.Monad
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import MiniScheme.Interpreter.Data
import MiniScheme.Interpreter.Monad

builtinEnv :: MonadInterp m => m (Env' m)
builtinEnv = do
  env <- rootEnv

  bind env "number?" $
    Proc env \_ args -> case args of
      [Int _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  bind env "boolean?" $
    Proc env \_ args -> case args of
      [Bool _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  bind env "string?" $
    Proc env \_ args -> case args of
      [Str _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  bind env "procedure?" $
    Proc env \_ args -> case args of
      [Proc _ _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  bind env "+" $
    Proc env \_ args -> Int . sum <$!> traverse expectInt args

  bind env "-" $
    Proc env \_ args ->
      traverse expectInt args >>= \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n - sum ns)

  bind env "*" $
    Proc env \_ args -> Int . product <$!> traverse expectInt args

  bind env "/" $
    Proc env \_ args ->
      traverse expectInt args >>= \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n `div` product ns)

  bind env "=" $
    Proc env \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 == n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env ">" $
    Proc env \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 > n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env ">=" $
    Proc env \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 >= n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env "<" $
    Proc env \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 < n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env "<=" $
    Proc env \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 <= n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env "not" $
    Proc env \_ args -> case args of
      [v] -> Bool . not <$!> expectBool v
      _ -> throw (EvalError "illegal number of arguments")

  bind env "string-append" $
    Proc env $ \_ args -> Str . Text.concat <$!> traverse expectStr args

  bind env "string->number" $
    Proc env \_ args -> case args of
      [v] ->
        expectStr v >>= \s -> case Text.signed Text.decimal s of
          Right (n, "") -> pure $! Int n
          _ -> throw (EvalError "Failed to convert string->number")
      _ -> throw (EvalError "illegal number of arguments")

  bind env "number->string" $
    Proc env \_ args -> case args of
      [v] -> expectInt v >>= \n -> pure $! Str (Text.pack (show n))
      _ -> throw (EvalError "illegal number of arguments")

  pure env
