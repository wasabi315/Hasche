{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Evaluator.Builtins
  ( builtinEnv,
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Text qualified as Text
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Monad
import MiniScheme.Parser (parseNum)

builtinEnv :: (MonadIO m, MonadThrow m, MonadEval n) => m (Env' n)
builtinEnv = do
  env <- rootEnv

  bind env "number?" $
    Proc env \_ args -> case args of
      [Num _] -> pure $! Bool True
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
    Proc env \_ args -> Num . sum <$!> traverse expectNum args

  bind env "-" $
    Proc env \_ args ->
      traverse expectNum args >>= \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Num (n - sum ns)

  bind env "*" $
    Proc env \_ args -> Num . product <$!> traverse expectNum args

  bind env "/" $
    Proc env \_ args ->
      traverse expectNum args >>= \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Num (n `div` product ns)

  bind env "=" $
    Proc env \_ args ->
      traverse expectNum args >>= \case
        [n1, n2] -> pure $! Bool (n1 == n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env ">" $
    Proc env \_ args ->
      traverse expectNum args >>= \case
        [n1, n2] -> pure $! Bool (n1 > n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env ">=" $
    Proc env \_ args ->
      traverse expectNum args >>= \case
        [n1, n2] -> pure $! Bool (n1 >= n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env "<" $
    Proc env \_ args ->
      traverse expectNum args >>= \case
        [n1, n2] -> pure $! Bool (n1 < n2)
        _ -> throw (EvalError "illegal number of arguments")

  bind env "<=" $
    Proc env \_ args ->
      traverse expectNum args >>= \case
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
        expectStr v >>= \s -> case parseNum s of
          Just n -> pure $! Num n
          Nothing -> throw (EvalError "Failed to convert string->number")
      _ -> throw (EvalError "illegal number of arguments")

  bind env "number->string" $
    Proc env \_ args -> case args of
      [v] -> expectNum v >>= \n -> pure $! Str (Text.pack (show n))
      _ -> throw (EvalError "illegal number of arguments")

  pure env
