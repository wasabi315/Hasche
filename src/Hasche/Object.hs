{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Hasche.Object
  ( Object,
    ObjLoc,
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
    prim,
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
    pattern Prim,
    pattern Func,
    pattern Cont,
    loc,
    ObjRef,
    deref,
    (.=),
    fromSExpr,
    toSExpr,
    Env,
    rootEnv,
    childEnv,
    lookup,
    bind,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import GHC.IO.Unsafe
import Hasche.SExpr
import System.IO
import System.Mem.StableName
import Text.StringRandom
import Prelude hiding (lookup)

-- data types

-- for variable and cons cell
type ObjRef m = IORef (Object m)

-- for pointer equality
type ObjLoc m = StableName (ObjKind m)

data Object m = Object_ (ObjKind m) (ObjLoc m)

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
  | Prim_ (Env m -> [Object m] -> m (Object m))
  | Func_ (Env m) (Env m -> [Object m] -> m (Object m))
  | Cont_ (Object m -> m (Object m))

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

alloc :: MonadIO m => ObjKind n -> m (Object n)
alloc k = Object_ k <$!> liftIO (makeStableName k)

-- allocate only once
undef, empty, true, false :: Object m
undef = unsafePerformIO (alloc Undef_)
empty = unsafePerformIO (alloc Empty_)
true = unsafePerformIO (alloc $! Bool_ True)
false = unsafePerformIO (alloc $! Bool_ False)
{-# NOINLINE undef #-}
{-# NOINLINE empty #-}
{-# NOINLINE true #-}
{-# NOINLINE false #-}

num :: MonadIO m => Integer -> m (Object n)
num n = liftIO . alloc $! Num_ n

str :: MonadIO m => Text -> m (Object n)
str s = liftIO . alloc $! Str_ s

-- May create new symbol
sym :: MonadIO m => Text -> m (Object n)
sym s = liftIO $
  HT.mutateIO _symtbl s \case
    Just obj -> pure (Just obj, obj)
    Nothing -> do
      obj <- alloc $! Sym_ s
      pure (Just obj, obj)

gensym :: MonadIO m => m (Object n)
gensym = do
  s <- liftIO $ stringRandomIO "#G\\d\\d\\d\\d\\d"
  mo <- liftIO $ HT.mutateIO _symtbl s \case
    Just obj -> pure (Just obj, Nothing)
    Nothing -> do
      obj <- alloc $! Sym_ s
      pure (Just obj, Just obj)
  maybe gensym pure mo

port :: MonadIO m => Handle -> m (Object n)
port h = liftIO . alloc $! Port_ h

cons :: MonadIO m => Object n -> Object n -> m (Object n)
cons car cdr = liftIO do
  ref1 <- newIORef car
  ref2 <- newIORef cdr
  alloc $! Cons_ ref1 ref2

syn :: MonadIO m => (Env n -> [SExpr] -> n (Object n)) -> m (Object n)
syn f = liftIO . alloc $! Syn_ f

prim :: MonadIO m => (Env n -> [Object n] -> n (Object n)) -> m (Object n)
prim f = liftIO . alloc $! Prim_ f

func :: MonadIO m => Env n -> (Env n -> [Object n] -> n (Object n)) -> m (Object n)
func e f = liftIO . alloc $! Func_ e f

cont :: MonadIO m => (Object n -> n (Object n)) -> m (Object n)
cont k = liftIO . alloc $! Cont_ k

-- object destructors

pattern Undef, Empty :: Object m
pattern Undef <- Object_ Undef_ _
pattern Empty <- Object_ Empty_ _

pattern Bool :: Bool -> Object m
pattern Bool b <- Object_ (Bool_ b) _

pattern Num :: Integer -> Object m
pattern Num n <- Object_ (Num_ n) _

pattern Str :: Text -> Object m
pattern Str s <- Object_ (Str_ s) _

pattern Sym :: Text -> Object m
pattern Sym s <- Object_ (Sym_ s) _

pattern Port :: Handle -> Object m
pattern Port h <- Object_ (Port_ h) _

pattern Cons :: ObjRef m -> ObjRef m -> Object m
pattern Cons r1 r2 <- Object_ (Cons_ r1 r2) _

pattern Syn :: (Env m -> [SExpr] -> m (Object m)) -> Object m
pattern Syn f <- Object_ (Syn_ f) _

pattern Prim :: (Env m -> [Object m] -> m (Object m)) -> Object m
pattern Prim f <- Object_ (Prim_ f) _

pattern Func :: Env m -> (Env m -> [Object m] -> m (Object m)) -> Object m
pattern Func e f <- Object_ (Func_ e f) _

pattern Cont :: (Object m -> m (Object m)) -> Object m
pattern Cont k <- Object_ (Cont_ k) _

{-# COMPLETE Undef, Empty, Bool, Num, Str, Sym, Port, Cons, Syn, Prim, Func, Cont #-}

loc :: Object n -> ObjLoc n
loc (Object_ _ l) = l

-- utils

toSExpr :: MonadIO m => Object n -> m (Maybe SExpr)
toSExpr = \case
  Empty -> pure . Just $! SList [] Nothing
  Bool b -> pure . Just $! SBool b
  Num n -> pure . Just $! SNum n
  Str s -> pure . Just $! SStr s
  Sym s -> pure . Just $! SSym s
  Cons car cdr -> do
    mx <- deref car >>= toSExpr
    my <- deref cdr >>= toSExpr
    case (mx, my) of
      (Just x, Just y) ->
        case y of
          SList es me -> pure . Just $! SList (x : es) me
          _ -> pure . Just $! SList [] (Just y)
      _ -> pure Nothing
  _ -> pure Nothing

fromSExpr :: MonadIO m => SExpr -> m (Object n)
fromSExpr (SBool b) = pure if b then true else false
fromSExpr (SNum n) = num n
fromSExpr (SStr s) = str s
fromSExpr (SSym s) = sym s
fromSExpr (SList es me) = do
  os <- traverse fromSExpr es
  mo <- traverse fromSExpr me
  foldrM cons (fromMaybe empty mo) os

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

bind :: MonadIO m => Env n -> Text -> Object n -> m ()
bind e i x = liftIO $ HT.insert (binds e) i =<< newIORef x
