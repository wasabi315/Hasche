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
    val,
    loc,
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
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IO.Unsafe
import MiniScheme.AST qualified as AST
import System.Mem.StableName
import Prelude hiding (lookup)

data Value m = Value (ValueKind m) (StableName (ValueKind m))

val :: Value m -> ValueKind m
val (Value v _) = v

loc :: Value m -> StableName (ValueKind m)
loc (Value _ l) = l

alloc :: MonadIO m => ValueKind n -> m (Value n)
alloc v = Value v <$!> liftIO (makeStableName v)

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
  | Pair (IORef (Value m)) (IORef (Value m))
  | Bool Bool
  | Num Number
  | Str Text
  | Sym Symbol
  | Prim (Env m -> [Value m] -> m (Value m))
  | Proc (Env m) (Env m -> [Value m] -> m (Value m))
  | Cont (Value m -> m (Value m))

type Number = AST.Number

prettyValue :: Value m -> IO String
prettyValue = fmap ($ "") . prettyValue' . val
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
        s1 <- readIORef r1 >>= prettyValue' . val
        readIORef r2 >>= \v -> case val v of
          Empty -> pure s1
          pair@(Pair _ _) -> (\s2 -> s1 . showChar ' ' . s2) <$> prettyPairs pair
          v' -> (\s2 -> s1 . showString " . " . s2) <$> prettyValue' v'
      v -> prettyValue' v

expectNum :: MonadThrow m => Value m -> m Integer
expectNum v = case val v of
  Num n -> pure n
  Undef -> throw (EvalError "undefined value evaluated")
  _ -> throw (EvalError "expect number")

expectPair :: MonadThrow m => Value m -> m (IORef (Value m), IORef (Value m))
expectPair v = case val v of
  Pair r1 r2 -> pure (r1, r2)
  Undef -> throw (EvalError "undefined value evaluated")
  _ -> throw (EvalError "expect pair")

expectBool :: MonadThrow m => Value m -> m Bool
expectBool v = case val v of
  Bool b -> pure b
  Undef -> throw (EvalError "undefined value evaluated")
  _ -> throw (EvalError "expect boolean")

expectStr :: MonadThrow m => Value m -> m Text
expectStr v = case val v of
  Str s -> pure s
  Undef -> throw (EvalError "undefined value evaluated")
  _ -> throw (EvalError "expect boolean")

expectSym :: MonadThrow m => Value m -> m Symbol
expectSym v = case val v of
  Sym s -> pure s
  Undef -> throw (EvalError "undefined value evaluated")
  _ -> throw (EvalError "expect boolean")

expectProc ::
  MonadThrow m =>
  Value m ->
  m (Env m, Env m -> [Value m] -> m (Value m))
expectProc v = case val v of
  Proc e f -> pure (e, f)
  Undef -> throw (EvalError "undefined value evaluated")
  _ -> throw (EvalError "expect boolean")

data Env m = Env
  { binds :: BasicHashTable AST.Id (IORef (Value m)),
    parent :: Maybe (Env m)
  }

rootEnv :: MonadIO m => m (Env n)
rootEnv = flip Env Nothing <$!> liftIO HT.new

childEnv :: MonadIO m => Env n -> m (Env n)
childEnv parent = flip Env (Just parent) <$!> liftIO HT.new

lookup :: (MonadIO m, MonadThrow m) => Env n -> AST.Id -> m (IORef (Value n))
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
    Nothing -> (,False) . Just <$> newIORef v
  when declared do
    throw (EvalError "identifier already declared")

set :: (MonadIO m, MonadThrow m) => Env n -> AST.Id -> Value n -> m ()
set e i v = do
  ref <- lookup e i
  liftIO $ modifyIORef' ref (const v)

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
