{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Evaluator
  ( newEvaluator,
    Value,
    EvalError,
  )
where

import Control.Exception.Safe
import MiniScheme.AST qualified as AST
import MiniScheme.Evaluator.Builtins
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Eval
import MiniScheme.Evaluator.Monad

newEvaluator :: IO ([AST.Prog] -> IO (Either EvalError Value))
newEvaluator = do
  env <- builtinEnv

  pure \prog ->
    catch
      (Right . Value <$> runEvaluator (eval env prog))
      (pure . Left)

data Value = forall m. Value (Value' m)

instance Show Value where
  show (Value v) = show v