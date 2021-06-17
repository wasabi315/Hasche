{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Hasche.Eval.Object
  ( Object,
    ObjRef,
    Env,
    undef,
    empty,
    true,
    false,
    num,
    str,
    sym,
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
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Box (Box)
import Data.Box qualified as Box
import Data.Cell (Cell)
import Data.Cell qualified as Cell
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.Text (Text)
import GHC.IO.Unsafe
import Language.Hasche.Eval.Env qualified as Env
import Language.Hasche.Syntax.SExpr
import System.IO

-- data types

type ObjRef m = Cell (Object m)

type Object m = Box (ObjKind m)

type Env m = Env.Env Text (Object m)

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

-- object constructors

-- allocate only once
undef, empty, true, false :: Object m
undef = unsafePerformIO (Box.new Undef_)
empty = unsafePerformIO (Box.new Empty_)
true = unsafePerformIO (Box.new $ Bool_ True)
false = unsafePerformIO (Box.new $ Bool_ False)
{-# NOINLINE undef #-}
{-# NOINLINE empty #-}
{-# NOINLINE true #-}
{-# NOINLINE false #-}

num :: MonadIO m => Integer -> m (Object n)
num n = Box.new $ Num_ n

str :: MonadIO m => Text -> m (Object n)
str s = Box.new $ Str_ s

-- Maps symbol name to object
_symtbl :: BasicHashTable Text (Object m)
_symtbl = unsafePerformIO HT.new
{-# NOINLINE _symtbl #-}

-- May create new symbol
sym :: MonadIO m => Text -> m (Object n)
sym s = liftIO $
  HT.mutateIO _symtbl s \case
    Just obj -> pure (Just obj, obj)
    Nothing -> do
      obj <- Box.new $ Sym_ s
      pure (Just obj, obj)

port :: MonadIO m => Handle -> m (Object n)
port h = Box.new $ Port_ h

cons :: MonadIO m => Object n -> Object n -> m (Object n)
cons car cdr = do
  r1 <- Cell.new car
  r2 <- Cell.new cdr
  Box.new $ Cons_ r1 r2

syn :: MonadIO m => (Env n -> [SExpr] -> n (Object n)) -> m (Object n)
syn f = Box.new $ Syn_ f

func :: MonadIO m => ([Object n] -> n (Object n)) -> m (Object n)
func f = Box.new $ Func_ f

cont :: MonadIO m => (Object n -> n (Object n)) -> m (Object n)
cont k = Box.new $ Cont_ k

-- object destructors

pattern Undef, Empty :: Object m
pattern Undef <- (Box.deref -> Undef_)
pattern Empty <- (Box.deref -> Empty_)

pattern Bool :: Bool -> Object m
pattern Bool b <- (Box.deref -> Bool_ b)

pattern Num :: Integer -> Object m
pattern Num n <- (Box.deref -> Num_ n)

pattern Str :: Text -> Object m
pattern Str s <- (Box.deref -> Str_ s)

pattern Sym :: Text -> Object m
pattern Sym s <- (Box.deref -> Sym_ s)

pattern Port :: Handle -> Object m
pattern Port h <- (Box.deref -> Port_ h)

pattern Cons :: ObjRef m -> ObjRef m -> Object m
pattern Cons r1 r2 <- (Box.deref -> Cons_ r1 r2)

pattern Syn :: (Env m -> [SExpr] -> m (Object m)) -> Object m
pattern Syn f <- (Box.deref -> Syn_ f)

pattern Func :: ([Object m] -> m (Object m)) -> Object m
pattern Func f <- (Box.deref -> Func_ f)

pattern Cont :: (Object m -> m (Object m)) -> Object m
pattern Cont k <- (Box.deref -> Cont_ k)

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
    mx <- Cell.deref car >>= toSExpr
    my <- Cell.deref cdr >>= toSExpr
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
