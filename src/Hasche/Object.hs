{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Hasche.Object
  ( Object,
    undef,
    empty,
    true,
    false,
    num,
    str,
    sym,
    cons,
    syn,
    prim,
    func,
    cont,
    pattern Undef,
    pattern Empty,
    pattern Bool,
    pattern Num,
    pattern Str,
    pattern Sym,
    pattern Cons,
    pattern Syn,
    pattern Prim,
    pattern Func,
    pattern Cont,
    ObjRef,
    deref,
    (.=),
    Env,
    rootEnv,
    childEnv,
    lookup,
    bind,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.IORef
import Data.Text (Text)
import GHC.IO.Unsafe
import Hasche.SExpr
import Prelude hiding (lookup)

-- data types

type ObjRef m = IORef (Object m)

data Object m
  = Undef_
  | Empty_
  | Bool_ Bool
  | Num_ Integer
  | Str_ Text
  | Sym_ Text
  | Cons_ (ObjRef m) (ObjRef m)
  | Syn_ (Env m -> [SExpr] -> m (ObjRef m))
  | Prim_ (Env m -> [ObjRef m] -> m (ObjRef m))
  | Func_ (Env m) (Env m -> [ObjRef m] -> m (ObjRef m))
  | Cont_ (ObjRef m -> m (ObjRef m))

type SymTable m = BasicHashTable Text (ObjRef m)

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
undef, empty, true, false :: ObjRef m
undef = unsafePerformIO (newIORef Undef_)
empty = unsafePerformIO (newIORef Empty_)
true = unsafePerformIO (newIORef $! Bool_ True)
false = unsafePerformIO (newIORef $! Bool_ False)
{-# NOINLINE undef #-}
{-# NOINLINE empty #-}
{-# NOINLINE true #-}
{-# NOINLINE false #-}

num :: MonadIO m => Integer -> m (ObjRef n)
num n = liftIO . newIORef $! Num_ n

str :: MonadIO m => Text -> m (ObjRef n)
str s = liftIO . newIORef $! Str_ s

-- May create new symbol
sym :: MonadIO m => Text -> m (ObjRef n)
sym s = liftIO $
  HT.mutateIO _symtbl s \case
    Just obj -> pure (Just obj, obj)
    Nothing -> do
      obj <- newIORef $! Sym_ s
      pure (Just obj, obj)

cons :: MonadIO m => ObjRef n -> ObjRef n -> m (ObjRef n)
cons car cdr = liftIO . newIORef $! Cons_ car cdr

syn :: MonadIO m => (Env n -> [SExpr] -> n (ObjRef n)) -> m (ObjRef n)
syn f = liftIO . newIORef $! Syn_ f

prim :: MonadIO m => (Env n -> [ObjRef n] -> n (ObjRef n)) -> m (ObjRef n)
prim f = liftIO . newIORef $! Prim_ f

func :: MonadIO m => Env n -> (Env n -> [ObjRef n] -> n (ObjRef n)) -> m (ObjRef n)
func e f = liftIO . newIORef $! Func_ e f

cont :: MonadIO m => (ObjRef n -> n (ObjRef n)) -> m (ObjRef n)
cont k = liftIO . newIORef $! Cont_ k

-- object destructors

pattern Undef, Empty :: Object m
pattern Undef <- Undef_
pattern Empty <- Empty_

pattern Bool :: Bool -> Object m
pattern Bool b <- Bool_ b

pattern Num :: Integer -> Object m
pattern Num n <- Num_ n

pattern Str :: Text -> Object m
pattern Str s <- Str_ s

pattern Sym :: Text -> Object m
pattern Sym s <- Sym_ s

pattern Cons :: ObjRef m -> ObjRef m -> Object m
pattern Cons r1 r2 <- Cons_ r1 r2

pattern Syn :: (Env m -> [SExpr] -> m (ObjRef m)) -> Object m
pattern Syn f <- Syn_ f

pattern Prim :: (Env m -> [ObjRef m] -> m (ObjRef m)) -> Object m
pattern Prim f <- Prim_ f

pattern Func :: Env m -> (Env m -> [ObjRef m] -> m (ObjRef m)) -> Object m
pattern Func e f <- Func_ e f

pattern Cont :: (ObjRef m -> m (ObjRef m)) -> Object m
pattern Cont k <- Cont_ k

{-# COMPLETE Undef, Empty, Bool, Num, Str, Sym, Cons, Syn, Prim, Func, Cont #-}

-- utils

deref :: MonadIO m => ObjRef n -> m (Object n)
deref = liftIO . readIORef

(.=) :: MonadIO m => ObjRef n -> Object n -> m ()
r .= v = liftIO (modifyIORef' r (const v))

infix 0 .=

-- Env methods

rootEnv :: MonadIO m => m (Env n)
rootEnv = flip Env Nothing <$!> liftIO HT.new

childEnv :: MonadIO m => Env n -> m (Env n)
childEnv env = flip Env (Just env) <$!> liftIO HT.new

lookup :: MonadIO m => Env n -> Text -> m (Maybe (ObjRef n))
lookup e i = lookup' e
  where
    lookup' Env {..} = do
      liftIO (HT.lookup binds i) >>= \case
        Just v -> pure (Just v)
        Nothing -> case parent of
          Just env' -> lookup' env'
          Nothing -> pure Nothing

bind :: MonadIO m => Env n -> Text -> ObjRef n -> m ()
bind e i x = liftIO $ HT.insert (binds e) i x
