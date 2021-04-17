{-# LANGUAGE ConstraintKinds #-}

module MiniScheme.Evaluator.Monad
  ( MonadEval,
    Evaluator,
    runEvaluator,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class

type MonadEval m = (MonadThrow m, MonadIO m)

type Evaluator = IO

runEvaluator :: Evaluator a -> IO a
runEvaluator = id
