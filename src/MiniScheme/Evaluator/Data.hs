{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Evaluator.Data
  ( Value' (..),
    Number,
    expectBool,
    expectNum,
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
import Prelude hiding (lookup)

data Value' m
  = Undef
  | Empty
  | Num Number
  | Bool Bool
  | Str Text
  | Proc (Env' m) (Env' m -> [Value' m] -> m (Value' m))

type Number = AST.Number

instance Show (Value' m) where
  show Undef = "undefined"
  show Empty = "()"
  show (Num n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show (Str s) = show s
  show (Proc _ _) = "<procedure>"

expectNum :: MonadThrow m => Value' m -> m Integer
expectNum (Num n) = pure n
expectNum Undef = throw (EvalError "undefined value evaluated")
expectNum _ = throw (EvalError "expect number")

expectBool :: MonadThrow m => Value' m -> m Bool
expectBool (Bool b) = pure b
expectBool Undef = throw (EvalError "undefined value evaluated")
expectBool _ = throw (EvalError "expect boolean")

expectStr :: MonadThrow m => Value' m -> m Text
expectStr (Str s) = pure s
expectStr Undef = throw (EvalError "undefined value evaluated")
expectStr _ = throw (EvalError "expect string")

expectProc ::
  MonadThrow m =>
  Value' m ->
  m (Env' m, Env' m -> [Value' m] -> m (Value' m))
expectProc (Proc e f) = pure (e, f)
expectProc Undef = throw (EvalError "undefined value evaluated")
expectProc _ = throw (EvalError "expect procedure")

data Env' m = Env'
  { binds :: BasicHashTable AST.Id (Value' m),
    parent :: Maybe (Env' m)
  }

rootEnv :: MonadIO m => m (Env' n)
rootEnv = flip Env' Nothing <$!> liftIO HT.new

childEnv :: MonadIO m => Env' n -> m (Env' n)
childEnv parent = flip Env' (Just parent) <$!> liftIO HT.new

lookup :: (MonadIO m, MonadThrow m) => Env' n -> AST.Id -> m (Value' n)
lookup env i = lookup' env
  where
    lookup' Env' {..} = do
      liftIO (HT.lookup binds i) >>= \case
        Just v -> pure v
        Nothing -> case parent of
          Just env' -> lookup' env'
          Nothing -> throw (EvalError $ "Unbound identifier: " <> i)

bind :: (MonadIO m, MonadThrow m) => Env' n -> AST.Id -> Value' n -> m ()
bind Env' {..} i v = do
  declared <- liftIO $ HT.mutate binds i \case
    Just v' -> (Just v', True)
    Nothing -> (Just v, False)
  when declared do
    throw (EvalError "identifier already declared")

set :: (MonadIO m, MonadThrow m) => Env' n -> AST.Id -> Value' n -> m ()
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
