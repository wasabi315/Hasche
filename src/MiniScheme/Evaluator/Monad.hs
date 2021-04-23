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
import Control.Monad.Cont
import Control.Monad.Reader
import MiniScheme.Evaluator.Data

type MonadEval m =
  ( MonadReader (SymTable m) m,
    MonadCont m,
    MonadThrow m,
    MonadIO m
  )

newtype Evaluator r a = Evaluator (ReaderT (SymTable (Evaluator r)) (ContT r IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadReader (SymTable (Evaluator r)),
      MonadCont
    )

runEvaluator :: Evaluator r a -> SymTable (Evaluator r) -> (a -> IO r) -> IO r
runEvaluator (Evaluator m) symtbl = runContT (runReaderT m symtbl)
