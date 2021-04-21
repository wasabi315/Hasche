{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MiniScheme.Evaluator.Monad
  ( MonadEval,
    Evaluator,
    runEvaluator,
    MonadReader (..),
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import MiniScheme.Evaluator.Data

type MonadEval m = (MonadReader SymTable m, MonadThrow m, MonadIO m)

type Evaluator = ReaderT SymTable IO

runEvaluator :: SymTable -> Evaluator a -> IO a
runEvaluator symtbl m = runReaderT m symtbl
