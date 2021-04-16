{-# LANGUAGE ConstraintKinds #-}

module MiniScheme.Interpreter.Monad
  ( MonadInterp,
    Interpreter,
    runInterpreter,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class

type MonadInterp m = (MonadThrow m, MonadIO m)

type Interpreter = IO

runInterpreter :: Interpreter a -> IO a
runInterpreter = id
