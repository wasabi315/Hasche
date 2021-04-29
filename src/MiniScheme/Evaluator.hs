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
    SomeValue,
    pretty,
    EvalError,
  )
where

import Control.Exception.Safe
import MiniScheme.AST qualified as AST
import MiniScheme.Evaluator.Builtins
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Eval
import MiniScheme.Evaluator.Monad

newEvaluator :: IO ([AST.Prog] -> IO (Either EvalError SomeValue))
newEvaluator = do
  env <- builtinEnv

  pure \prog ->
    catch
      (runEvaluator (eval env prog) (pure . Right . Value))
      (pure . Left)

data SomeValue = forall m. Value (Value m)

pretty :: SomeValue -> IO String
pretty (Value v) = prettyValue v
