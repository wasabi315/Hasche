{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Interpreter.Data
  ( Value' (..),
    expectBool,
    expectInt,
    expectStr,
    expectProc,
    Env',
    rootEnv,
    childEnv,
    lookup,
    bind,
    set,
    EvalError (..),
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import MiniScheme.AST qualified as AST
import MiniScheme.Interpreter.Monad
import Prelude hiding (lookup)

data Value' m
  = Int Integer
  | Bool Bool
  | Str Text
  | Proc (Env' m) (Env' m -> [Value' m] -> m (Value' m))

instance Show (Value' m) where
  show (Int n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show (Str s) = show s
  show (Proc _ _) = "<procedure>"

expectInt :: MonadInterp m => Value' m -> m Integer
expectInt (Int n) = pure n
expectInt _ = throw (EvalError "expect number")

expectBool :: MonadInterp m => Value' m -> m Bool
expectBool (Bool b) = pure b
expectBool _ = throw (EvalError "expect boolean")

expectStr :: MonadInterp m => Value' m -> m Text
expectStr (Str s) = pure s
expectStr _ = throw (EvalError "expect string")

expectProc ::
  MonadInterp m =>
  Value' m ->
  m (Env' m, Env' m -> [Value' m] -> m (Value' m))
expectProc (Proc e f) = pure (e, f)
expectProc _ = throw (EvalError "expect procedure")

data Env' m = Env'
  { binds :: BasicHashTable AST.Id (Value' m),
    parent :: Maybe (Env' m)
  }

rootEnv :: MonadInterp m => m (Env' m)
rootEnv = flip Env' Nothing <$!> liftIO HT.new

childEnv :: MonadInterp m => Env' m -> m (Env' m)
childEnv parent = flip Env' (Just parent) <$!> liftIO HT.new

lookup :: MonadInterp m => Env' m -> AST.Id -> m (Value' m)
lookup env i = lookup' env
  where
    lookup' Env' {..} = do
      liftIO (HT.lookup binds i) >>= \case
        Just v -> pure v
        Nothing -> case parent of
          Just env' -> lookup' env'
          Nothing -> throw (EvalError $ "Unbound identifier: " <> i)

bind :: MonadInterp m => Env' m -> AST.Id -> Value' m -> m ()
bind Env' {..} i v = do
  declared <- liftIO $ HT.mutate binds i \case
    Just v' -> (Just v', True)
    Nothing -> (Just v, False)
  when declared do
    throw (EvalError "identifier already declared")

set :: MonadInterp m => Env' m -> AST.Id -> Value' m -> m ()
set env i v = set' env
  where
    set' Env' {..} = do
      done <- liftIO $ HT.mutate binds i \case
        Just _ -> (Just v, True)
        Nothing -> (Nothing, False)
      unless done do
        case parent of
          Just env' -> set' env'
          Nothing -> throw (EvalError "Unbound identifier")

newtype EvalError = EvalError Text

instance Show EvalError where
  show (EvalError reason) = Text.unpack reason

instance Exception EvalError
