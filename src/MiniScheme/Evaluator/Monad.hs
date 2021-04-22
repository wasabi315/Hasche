{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

type MonadEval m = (MonadReader (SymTable m) m, MonadThrow m, MonadIO m)

newtype Evaluator a = Evaluator (ReaderT (SymTable Evaluator) IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadReader (SymTable Evaluator)
    )

runEvaluator :: SymTable Evaluator -> Evaluator a -> IO a
runEvaluator symtbl (Evaluator m) = runReaderT m symtbl
