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
import Control.Monad.Cont
import Data.Foldable
import Data.IORef
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Eval
import MiniScheme.Evaluator.Monad
import MiniScheme.Parser

builtinEnv :: (MonadIO m, MonadThrow m, MonadEval n) => m (Env n)
builtinEnv = do
  env <- rootEnv

  traverse_
    (\(i, v) -> bind env i =<< v)
    [ ( "null?",
        proc1 \_ v -> case val v of
          Empty -> pure true
          _ -> pure false
      ),
      ( "pair?",
        proc1 \_ v -> case val v of
          Pair _ _ -> pure true
          _ -> pure false
      ),
      ( "number?",
        proc1 \_ v -> case val v of
          Num _ -> pure true
          _ -> pure false
      ),
      ( "boolean?",
        proc1 \_ v -> case val v of
          Bool _ -> pure true
          _ -> pure false
      ),
      ( "string?",
        proc1 \_ v -> case val v of
          Str _ -> pure true
          _ -> pure false
      ),
      ( "symbol?",
        proc1 \_ v -> case val v of
          Sym _ -> pure true
          _ -> pure false
      ),
      ( "procedure?",
        proc1 \_ v -> case val v of
          Proc _ _ -> pure true
          Prim _ -> pure true
          _ -> pure false
      ),
      ("+", numFold (+) 0),
      ("*", numFold (*) 1),
      ( "-",
        builtin . const $
          traverse expectNum >=> \case
            [] -> throw (EvalError "expect at least one number")
            n : ns -> alloc $ Num (n - sum ns)
      ),
      ( "/",
        builtin . const $
          traverse expectNum >=> \case
            [] -> throw (EvalError "expect at least one number")
            n : ns -> alloc $ Num (n `div` product ns)
      ),
      ("=", numBinPred (==)),
      (">", numBinPred (>)),
      (">=", numBinPred (>=)),
      ("<", numBinPred (<)),
      ("<=", numBinPred (<=)),
      ( "string-append",
        builtin . const $ traverse expectStr >=> alloc . Str . Text.concat
      ),
      ( "string->number",
        proc1 . const $
          expectStr >=> \s -> case parseNum s of
            Just n -> alloc $ Num n
            Nothing -> throw (EvalError "Failed to convert string->number")
      ),
      ( "number->string",
        proc1 . const $ expectNum >=> alloc . Str . Text.pack . show
      ),
      ( "string->symbol",
        proc1 . const $
          expectStr >=> \s -> do
            symtbl <- ask
            strToSym symtbl s
      ),
      ( "symbol->string",
        proc1 . const $ expectSym >=> alloc . Str . symToStr
      ),
      ( "eq?",
        proc2 \_ x y -> bool <$!> isEq x y
      ),
      ( "eqv?",
        proc2 \_ x y -> bool <$!> isEqv x y
      ),
      ( "equal?",
        proc2 \_ x y -> bool <$!> isEqual x y
      ),
      ( "cons",
        proc2 . const $ cons
      ),
      ( "car",
        proc1 . const $ expectPair >=> liftIO . readIORef . fst
      ),
      ( "cdr",
        proc1 . const $ expectPair >=> liftIO . readIORef . snd
      ),
      ( "set-car!",
        proc2 \_ v1 v2 -> do
          (r1, _) <- expectPair v1
          liftIO $ modifyIORef' r1 (const v2)
          pure undef
      ),
      ( "set-cdr!",
        proc2 \_ v1 v2 -> do
          (_, r2) <- expectPair v1
          liftIO $ modifyIORef' r2 (const v2)
          pure undef
      ),
      -- FIXME: hacky
      ( "eval",
        proc1 \env' v -> do
          liftIO (prettyValue v) >>= \str -> case parseProg "" (Text.pack str) of
            Right [p] -> eval env' [p]
            Left err -> throw (EvalError . Text.pack $ displayException err)
            _ -> throw (EvalError "failed to eval")
      ),
      ( "apply",
        builtin \env' -> \case
          [] -> throw (EvalError "illegal number of arguments")
          [_] -> throw (EvalError "illegal number of arguments")
          (f : xs) -> do
            args <- (init xs ++) <$!> pairToList (last xs)
            apply env' f args
      ),
      ( "call/cc",
        proc1 \env' v -> do
          callCC \k -> do
            c <- alloc $ Cont k
            apply env' v [c]
      ),
      ( "display",
        proc1 \_ v ->
          undef <$ liftIO case val v of
            Str str -> Text.putStr str
            _ -> prettyValue v >>= putStr
      ),
      ( "newline",
        proc0 . const $ undef <$ liftIO (putStrLn "")
      )
    ]

  pure env

builtin :: MonadIO m => (Env n -> [Value' n] -> n (Value' n)) -> m (Value' n)
builtin f = alloc $ Prim f

proc0 :: (MonadIO m, MonadEval n) => (Env n -> n (Value' n)) -> m (Value' n)
proc0 f =
  builtin \env -> \case
    [] -> f env
    _ -> throw (EvalError "illegal number of arguments")

proc1 :: (MonadIO m, MonadEval n) => (Env n -> Value' n -> n (Value' n)) -> m (Value' n)
proc1 f =
  builtin \env -> \case
    [v] -> f env v
    _ -> throw (EvalError "illegal number of arguments")

proc2 :: (MonadIO m, MonadEval n) => (Env n -> Value' n -> Value' n -> n (Value' n)) -> m (Value' n)
proc2 f =
  builtin \env -> \case
    [v1, v2] -> f env v1 v2
    _ -> throw (EvalError "illegal number of arguments")

numFold :: (MonadIO m, MonadEval n) => (Number -> Number -> Number) -> Number -> m (Value' n)
numFold f n =
  builtin . const $ traverse expectNum >=> alloc . Num . foldl' f n

numBinPred :: (MonadIO m, MonadEval n) => (Number -> Number -> Bool) -> m (Value' n)
numBinPred f =
  proc2 \_ v1 v2 -> do
    n1 <- expectNum v1
    n2 <- expectNum v2
    pure $! if f n1 n2 then true else false

bool :: Bool -> Value' m
bool b = if b then true else false

isEq :: MonadIO m => Value' m -> Value' m -> m Bool
isEq v w = pure $! loc v == loc w

isEqv :: MonadIO m => Value' m -> Value' m -> m Bool
isEqv x y = case (val x, val y) of
  (Num n1, Num n2) -> pure $! n1 == n2
  _ -> isEq x y

isEqual :: MonadIO m => Value' m -> Value' m -> m Bool
isEqual x y = case (val x, val y) of
  (Pair r1 r2, Pair r3 r4) -> do
    v1 <- liftIO (readIORef r1)
    v3 <- liftIO (readIORef r3)
    isEqual v1 v3 >>= \case
      False -> pure False
      True -> do
        v2 <- liftIO (readIORef r2)
        v4 <- liftIO (readIORef r4)
        isEqual v2 v4
  _ -> isEqv x y

pairToList :: MonadIO m => Value' m -> m [Value' m]
pairToList v = case val v of
  Empty -> pure []
  Pair r1 r2 -> do
    v1 <- liftIO (readIORef r1)
    v2 <- liftIO (readIORef r2)
    vs <- pairToList v2
    pure $! v1 : vs
  _ -> pure [v]
