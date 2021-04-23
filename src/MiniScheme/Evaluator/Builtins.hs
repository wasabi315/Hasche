{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.IORef
import Data.Text qualified as Text
import GHC.IO.Unsafe
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Eval
import MiniScheme.Evaluator.Monad
import MiniScheme.Parser (parseNum)

builtinEnv :: (MonadIO m, MonadThrow m, MonadEval n) => m (Env n)
builtinEnv = do
  env <- rootEnv

  traverse_
    (\(i, v) -> bind env i =<< v)
    [ ( "null?",
        proc1 \v -> case val v of
          Empty -> pure true
          _ -> pure false
      ),
      ( "pair?",
        proc1 \v -> case val v of
          Pair _ _ -> pure true
          _ -> pure false
      ),
      ( "number?",
        proc1 \v -> case val v of
          Num _ -> pure true
          _ -> pure false
      ),
      ( "boolean?",
        proc1 \v -> case val v of
          Bool _ -> pure true
          _ -> pure false
      ),
      ( "string?",
        proc1 \v -> case val v of
          Str _ -> pure true
          _ -> pure false
      ),
      ( "symbol?",
        proc1 \v -> case val v of
          Sym _ -> pure true
          _ -> pure false
      ),
      ( "procedure?",
        proc1 \v -> case val v of
          Proc _ _ -> pure true
          _ -> pure false
      ),
      ("+", numFold (+) 0),
      ("*", numFold (*) 1),
      ( "-",
        builtin
          ( traverse expectNum >=> \case
              [] -> throw (EvalError "expect at least one number")
              n : ns -> alloc $ Num (n - sum ns)
          )
      ),
      ( "/",
        builtin
          ( traverse expectNum >=> \case
              [] -> throw (EvalError "expect at least one number")
              n : ns -> alloc $ Num (n `div` product ns)
          )
      ),
      ("=", numBinPred (==)),
      (">", numBinPred (>)),
      (">=", numBinPred (>=)),
      ("<", numBinPred (<)),
      ("<=", numBinPred (<=)),
      ( "string-append",
        builtin (traverse expectStr >=> alloc . Str . Text.concat)
      ),
      ( "string->number",
        proc1
          ( expectStr >=> \s -> case parseNum s of
              Just n -> alloc $ Num n
              Nothing -> throw (EvalError "Failed to convert string->number")
          )
      ),
      ( "number->string",
        proc1 (expectNum >=> alloc . Str . Text.pack . show)
      ),
      ( "string->symbol",
        proc1
          ( expectStr >=> \s -> do
              symtbl <- ask
              strToSym symtbl s
          )
      ),
      ( "symbol->string",
        proc1 (expectSym >=> alloc . Str . symToStr)
      ),
      ( "eq?",
        proc2 \v1 v2 ->
          pure $! if loc v1 == loc v2 then true else false
      ),
      ( "cons",
        proc2 cons
      ),
      ( "car",
        proc1 (expectPair >=> liftIO . readIORef . fst)
      ),
      ( "cdr",
        proc1 (expectPair >=> liftIO . readIORef . snd)
      ),
      ( "set-car!",
        proc2 \v1 v2 -> do
          (r1, _) <- expectPair v1
          liftIO $ modifyIORef' r1 (const v2)
          pure undef
      ),
      ( "set-cdr!",
        proc2 \v1 v2 -> do
          (_, r2) <- expectPair v1
          liftIO $ modifyIORef' r2 (const v2)
          pure undef
      ),
      ( "apply",
        builtin apply
      )
    ]

  pure env

builtin :: MonadIO m => ([Value' n] -> n (Value' n)) -> m (Value' n)
builtin f = alloc $ Proc emptyEnv (const f)

emptyEnv :: Env n
emptyEnv = unsafePerformIO rootEnv
{-# NOINLINE emptyEnv #-}

proc1 :: (MonadIO m, MonadEval n) => (Value' n -> n (Value' n)) -> m (Value' n)
proc1 f =
  builtin \case
    [v] -> f v
    _ -> throw (EvalError "illegal number of arguments")

proc2 :: (MonadIO m, MonadEval n) => (Value' n -> Value' n -> n (Value' n)) -> m (Value' n)
proc2 f =
  builtin \case
    [v1, v2] -> f v1 v2
    _ -> throw (EvalError "illegal number of arguments")

numFold :: (MonadIO m, MonadEval n) => (Number -> Number -> Number) -> Number -> m (Value' n)
numFold f n =
  builtin $ traverse expectNum >=> alloc . Num . foldl' f n

numBinPred :: (MonadIO m, MonadEval n) => (Number -> Number -> Bool) -> m (Value' n)
numBinPred f =
  proc2 \v1 v2 -> do
    n1 <- expectNum v1
    n2 <- expectNum v2
    pure $! if f n1 n2 then true else false
