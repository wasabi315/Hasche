{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module MiniScheme.Evaluator.Data
  ( Value,
    alloc,
    deref,
    (.=),
    undef,
    empty,
    true,
    false,
    ValueKind (..),
    prettyValue,
    Number,
    expectPair,
    expectBool,
    expectNum,
    expectStr,
    expectSym,
    expectProc,
    Env,
    rootEnv,
    childEnv,
    lookup,
    bind,
    set,
    Symbol,
    strToSym,
    symToStr,
    EvalError (..),
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IO.Unsafe
import MiniScheme.AST qualified as AST
import Prelude hiding (lookup)

type Value m = IORef (ValueKind m)

alloc :: MonadIO m => ValueKind n -> m (Value n)
alloc = liftIO . newIORef

deref :: MonadIO m => Value n -> m (ValueKind n)
deref = liftIO . readIORef

(.=) :: MonadIO m => Value n -> ValueKind n -> m ()
r .= x = liftIO $ modifyIORef' r (const x)

infix 0 .=

undef, empty, true, false :: Value m
undef = unsafePerformIO (alloc Undef)
empty = unsafePerformIO (alloc Empty)
true = unsafePerformIO (alloc (Bool True))
false = unsafePerformIO (alloc (Bool False))
{-# NOINLINE undef #-}
{-# NOINLINE empty #-}
{-# NOINLINE true #-}
{-# NOINLINE false #-}

data ValueKind m
  = Undef
  | Empty
  | Pair (Value m) (Value m)
  | Bool Bool
  | Num Number
  | Str Text
  | Sym Symbol
  | Prim (Env m -> [Value m] -> m (Value m))
  | Proc (Env m) (Env m -> [Value m] -> m (Value m))
  | Cont (Value m -> m (Value m))

type Number = AST.Number

prettyValue :: Value m -> IO String
prettyValue v = deref v >>= prettyValue' <&> ($ "")
  where
    prettyValue' = \case
      Undef -> pure $ showString "#<undef>"
      Empty -> pure $ showString "()"
      pair@(Pair _ _) -> showParen True <$> prettyPairs pair
      Num n -> pure $ shows n
      Bool b -> pure . showString $ if b then "#t" else "#f"
      Str s -> pure $ shows s
      Sym s -> pure $ shows s
      Prim _ -> pure $ showString "#<primitive>"
      Proc _ _ -> pure $ showString "#<procedure>"
      Cont _ -> pure $ showString "#<continuation>"

    prettyPairs = \case
      Pair r1 r2 -> do
        s1 <- deref r1 >>= prettyValue'
        deref r2 >>= \case
          Empty -> pure s1
          pair@(Pair _ _) -> (\s2 -> s1 . showChar ' ' . s2) <$> prettyPairs pair
          v' -> (\s2 -> s1 . showString " . " . s2) <$> prettyValue' v'
      v' -> prettyValue' v'

expectNum :: (MonadIO m, MonadThrow m) => Value m -> m Integer
expectNum v =
  deref v >>= \case
    Num n -> pure n
    Undef -> throw (EvalError "undefined value evaluated")
    _ -> throw (EvalError "expect number")

expectPair :: (MonadIO m, MonadThrow m) => Value m -> m (Value m, Value m)
expectPair v =
  deref v >>= \case
    Pair r1 r2 -> pure (r1, r2)
    Undef -> throw (EvalError "undefined value evaluated")
    _ -> throw (EvalError "expect pair")

expectBool :: (MonadIO m, MonadThrow m) => Value m -> m Bool
expectBool v =
  deref v >>= \case
    Bool b -> pure b
    Undef -> throw (EvalError "undefined value evaluated")
    _ -> throw (EvalError "expect boolean")

expectStr :: (MonadIO m, MonadThrow m) => Value m -> m Text
expectStr v =
  deref v >>= \case
    Str s -> pure s
    Undef -> throw (EvalError "undefined value evaluated")
    _ -> throw (EvalError "expect boolean")

expectSym :: (MonadIO m, MonadThrow m) => Value m -> m Symbol
expectSym v =
  deref v >>= \case
    Sym s -> pure s
    Undef -> throw (EvalError "undefined value evaluated")
    _ -> throw (EvalError "expect boolean")

expectProc ::
  (MonadIO m, MonadThrow m) =>
  Value m ->
  m (Env m, Env m -> [Value m] -> m (Value m))
expectProc v =
  deref v >>= \case
    Proc e f -> pure (e, f)
    Undef -> throw (EvalError "undefined value evaluated")
    _ -> throw (EvalError "expect boolean")

data Env m = Env
  { binds :: BasicHashTable AST.Id (Value m),
    parent :: Maybe (Env m)
  }

rootEnv :: MonadIO m => m (Env n)
rootEnv = flip Env Nothing <$!> liftIO HT.new

childEnv :: MonadIO m => Env n -> m (Env n)
childEnv parent = flip Env (Just parent) <$!> liftIO HT.new

lookup :: (MonadIO m, MonadThrow m) => Env n -> AST.Id -> m (Value n)
lookup env i = lookup' env
  where
    lookup' Env {..} = do
      liftIO (HT.lookup binds i) >>= \case
        Just v -> pure v
        Nothing -> case parent of
          Just env' -> lookup' env'
          Nothing -> throw (EvalError $ "Unbound identifier: " <> i)

bind :: (MonadIO m, MonadThrow m) => Env n -> AST.Id -> Value n -> m ()
bind Env {..} i v = do
  declared <- liftIO $ HT.mutateIO binds i \case
    Just v' -> pure (Just v', True)
    Nothing -> pure (Just v, False)
  when declared do
    throw (EvalError "identifier already declared")

set :: (MonadIO m, MonadThrow m) => Env n -> AST.Id -> Value n -> m ()
set e i v = do
  ref <- lookup e i
  x <- deref v
  ref .= x

newtype Symbol = Symbol Text

instance Show Symbol where
  show (Symbol s) = Text.unpack s

type SymTable m = BasicHashTable Text (Value m)

_symtbl :: SymTable m
_symtbl = unsafePerformIO HT.new
{-# NOINLINE _symtbl #-}

symToStr :: Symbol -> Text
symToStr (Symbol name) = name

-- may create new symbol
strToSym :: MonadIO m => Text -> m (Value n)
strToSym t = liftIO do
  HT.mutateIO _symtbl t \case
    Nothing -> do
      s <- alloc (Sym (Symbol t))
      pure (Just s, s)
    Just s -> pure (Just s, s)

newtype EvalError = EvalError Text

instance Show EvalError where
  show (EvalError reason) = Text.unpack reason

instance Exception EvalError
