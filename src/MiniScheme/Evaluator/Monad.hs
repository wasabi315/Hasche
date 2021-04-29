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

type MonadEval m =
  ( MonadCont m,
    MonadThrow m,
    MonadIO m
  )

type Evaluator r a = ContT r IO a

runEvaluator :: Evaluator r a -> (a -> IO r) -> IO r
runEvaluator = runContT
