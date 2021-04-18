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
import Data.Foldable
import Data.Text qualified as Text
import GHC.IO.Unsafe
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Monad
import MiniScheme.Parser (parseNum)

builtinEnv :: (MonadIO m, MonadThrow m, MonadEval n) => m (Env' n)
builtinEnv = do
  env <- rootEnv

  bind env "number?" $
    proc1 \case
      Num _ -> pure $! Bool True
      _ -> pure $! Bool False

  bind env "boolean?" $
    proc1 \case
      Bool _ -> pure $! Bool True
      _ -> pure $! Bool False

  bind env "string?" $
    proc1 \case
      Str _ -> pure $! Bool True
      _ -> pure $! Bool False

  bind env "procedure?" $
    proc1 \case
      Proc _ _ -> pure $! Bool True
      _ -> pure $! Bool False

  bind env "+" (numFold (+) 0)
  bind env "*" (numFold (*) 1)

  bind env "-" $
    builtin $
      traverse expectNum >=> \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Num (n - sum ns)

  bind env "/" $
    builtin $
      traverse expectNum >=> \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Num (n `div` product ns)

  bind env "=" (numBinPred (==))
  bind env ">" (numBinPred (>))
  bind env ">=" (numBinPred (>=))
  bind env "<" (numBinPred (<))
  bind env "<=" (numBinPred (<=))

  bind env "not" $
    proc1 \v -> Bool . not <$!> expectBool v

  bind env "string-append" $
    builtin \vs -> Str . Text.concat <$!> traverse expectStr vs

  bind env "string->number" $
    proc1 $
      expectStr >=> \s -> case parseNum s of
        Just n -> pure $! Num n
        Nothing -> throw (EvalError "Failed to convert string->number")

  bind env "number->string" $
    proc1 \v -> Str . Text.pack . show <$!> expectNum v

  pure env

builtin :: ([Value' m] -> m (Value' m)) -> Value' m
builtin f = Proc emptyEnv (const f)
  where
    emptyEnv = unsafePerformIO rootEnv
    {-# NOINLINE emptyEnv #-}

proc1 :: MonadEval m => (Value' m -> m (Value' m)) -> Value' m
proc1 f =
  builtin \case
    [v] -> f v
    _ -> throw (EvalError "illegal number of arguments")

proc2 :: MonadEval m => (Value' m -> Value' m -> m (Value' m)) -> Value' m
proc2 f =
  builtin \case
    [v1, v2] -> f v1 v2
    _ -> throw (EvalError "illegal number of arguments")

numFold :: MonadEval m => (Number -> Number -> Number) -> Number -> Value' m
numFold f n =
  builtin \vs -> Num . foldl' f n <$!> traverse expectNum vs

numBinPred :: MonadEval m => (Number -> Number -> Bool) -> Value' m
numBinPred f =
  proc2 \v1 v2 -> do
    n1 <- expectNum v1
    n2 <- expectNum v2
    pure $! Bool (f n1 n2)
