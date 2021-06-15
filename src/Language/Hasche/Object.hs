{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Hasche.Object
  ( Object,
    ObjRef,
    undef,
    empty,
    true,
    false,
    num,
    str,
    sym,
    gensym,
    port,
    cons,
    syn,
    func,
    cont,
    pattern Undef,
    pattern Empty,
    pattern Bool,
    pattern Num,
    pattern Str,
    pattern Sym,
    pattern Port,
    pattern Cons,
    pattern Syn,
    pattern Func,
    pattern Cont,
    fromSExpr,
    toSExpr,
    Env,
    rootEnv,
    childEnv,
    lookup,
    bind,
    module Language.Hasche.Box,
    module Language.Hasche.Cell,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.Maybe
import Data.Text (Text)
import GHC.IO.Unsafe
import Language.Hasche.Box
import Language.Hasche.Cell
import Language.Hasche.SExpr
import System.IO
import Text.StringRandom
import Prelude hiding (lookup)

-- data types

type ObjRef m = Cell (Object m)

type Object m = Box (ObjKind m)

data ObjKind m
  = Undef_
  | Empty_
  | Bool_ Bool
  | Num_ Integer
  | Str_ Text
  | Sym_ Text
  | Port_ Handle
  | Cons_ (ObjRef m) (ObjRef m)
  | Syn_ (Env m -> [SExpr] -> m (Object m))
  | Func_ ([Object m] -> m (Object m))
  | Cont_ (Object m -> m (Object m))

-- Symbol table

type SymTable m = BasicHashTable Text (Object m)

-- global symbol table
_symtbl :: SymTable m
_symtbl = unsafePerformIO HT.new
{-# NOINLINE _symtbl #-}

data Env m = Env
  { binds :: BasicHashTable Text (ObjRef m),
    parent :: Maybe (Env m)
  }

-- object constructors

-- allocate only once
undef, empty, true, false :: Object m
undef = unsafePerformIO (alloc Undef_)
empty = unsafePerformIO (alloc Empty_)
true = unsafePerformIO (alloc $ Bool_ True)
false = unsafePerformIO (alloc $ Bool_ False)
{-# NOINLINE undef #-}
{-# NOINLINE empty #-}
{-# NOINLINE true #-}
{-# NOINLINE false #-}

num :: MonadIO m => Integer -> m (Object n)
num n = liftIO . alloc $ Num_ n

str :: MonadIO m => Text -> m (Object n)
str s = liftIO . alloc $ Str_ s

-- May create new symbol
sym :: MonadIO m => Text -> m (Object n)
sym s = liftIO $
  HT.mutateIO _symtbl s \case
    Just obj -> pure (Just obj, obj)
    Nothing -> do
      obj <- alloc $ Sym_ s
      pure (Just obj, obj)

gensym :: MonadIO m => m (Object n)
gensym = do
  s <- liftIO $ stringRandomIO "#G\\d\\d\\d\\d\\d"
  mo <- liftIO $ HT.mutateIO _symtbl s \case
    Just obj -> pure (Just obj, Nothing)
    Nothing -> do
      obj <- alloc $ Sym_ s
      pure (Just obj, Just obj)
  maybe gensym pure mo

port :: MonadIO m => Handle -> m (Object n)
port h = liftIO . alloc $ Port_ h

cons :: MonadIO m => Object n -> Object n -> m (Object n)
cons car cdr = do
  r1 <- newCell car
  r2 <- newCell cdr
  alloc $ Cons_ r1 r2

syn :: MonadIO m => (Env n -> [SExpr] -> n (Object n)) -> m (Object n)
syn f = liftIO . alloc $ Syn_ f

func :: MonadIO m => ([Object n] -> n (Object n)) -> m (Object n)
func f = liftIO . alloc $ Func_ f

cont :: MonadIO m => (Object n -> n (Object n)) -> m (Object n)
cont k = liftIO . alloc $ Cont_ k

-- object destructors

pattern Undef, Empty :: Object m
pattern Undef <- (val -> Undef_)
pattern Empty <- (val -> Empty_)

pattern Bool :: Bool -> Object m
pattern Bool b <- (val -> Bool_ b)

pattern Num :: Integer -> Object m
pattern Num n <- (val -> Num_ n)

pattern Str :: Text -> Object m
pattern Str s <- (val -> Str_ s)

pattern Sym :: Text -> Object m
pattern Sym s <- (val -> Sym_ s)

pattern Port :: Handle -> Object m
pattern Port h <- (val -> Port_ h)

pattern Cons :: ObjRef m -> ObjRef m -> Object m
pattern Cons r1 r2 <- (val -> Cons_ r1 r2)

pattern Syn :: (Env m -> [SExpr] -> m (Object m)) -> Object m
pattern Syn f <- (val -> Syn_ f)

pattern Func :: ([Object m] -> m (Object m)) -> Object m
pattern Func f <- (val -> Func_ f)

pattern Cont :: (Object m -> m (Object m)) -> Object m
pattern Cont k <- (val -> Cont_ k)

{-# COMPLETE Undef, Empty, Bool, Num, Str, Sym, Port, Cons, Syn, Func, Cont #-}

-- utils

toSExpr :: MonadIO m => Object n -> m (Maybe SExpr)
toSExpr = \case
  Empty -> pure . Just $ SEmpty
  Bool b -> pure . Just $ SBool b
  Num n -> pure . Just $ SNum n
  Str s -> pure . Just $ SStr s
  Sym s -> pure . Just $ SSym s
  Cons car cdr -> do
    mx <- deref car >>= toSExpr
    my <- deref cdr >>= toSExpr
    pure $ liftM2 SCons mx my
  _ -> pure Nothing

fromSExpr :: MonadIO m => SExpr -> m (Object n)
fromSExpr SEmpty = pure empty
fromSExpr (SBool b) = pure if b then true else false
fromSExpr (SNum n) = num n
fromSExpr (SStr s) = str s
fromSExpr (SSym s) = sym s
fromSExpr (SCons e1 e2) = do
  o1 <- fromSExpr e1
  o2 <- fromSExpr e2
  cons o1 o2

-- Env methods

rootEnv :: MonadIO m => m (Env n)
rootEnv = flip Env Nothing <$> liftIO HT.new

childEnv :: MonadIO m => Env n -> m (Env n)
childEnv env = flip Env (Just env) <$> liftIO HT.new

lookup :: MonadIO m => Env n -> Text -> m (Maybe (ObjRef n))
lookup e i = lookup' e
  where
    lookup' Env {..} = do
      liftIO (HT.lookup binds i) >>= \case
        Just v -> pure (Just v)
        Nothing -> case parent of
          Just env' -> lookup' env'
          Nothing -> pure Nothing

bind :: MonadIO m => Env n -> Text -> Object n -> m ()
bind e i = newCell >=> liftIO . HT.insert (binds e) i
