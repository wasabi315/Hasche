{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Hasche.Object
  ( MonadIDSupply (..),
    MonadRef (..),
    MonadSym (..),
    Object,
    ObjRef,
    ObjKind,
    DWAction (..),
    undef,
    empty,
    true,
    false,
    num,
    str,
    sym,
    freshSym,
    port,
    cons,
    list,
    dlist,
    syn,
    func,
    cont,
    objID,
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
    ListifyResult (..),
    listify,
    unlistify,
  )
where

import Data.Foldable
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import System.IO

-- pseudo pointer equality
class (Monad m, Eq (ObjID m)) => MonadIDSupply m where
  type ObjID m :: Type
  undefID, emptyID, trueID, falseID :: ObjID m -- Reserved IDs
  freshID :: m (ObjID m)

-- mutable reference
class (Monad m) => MonadRef m where
  type Ref m :: Type -> Type
  newRef :: a -> m (Ref m a)
  deref :: Ref m a -> m a
  setRef :: Ref m a -> a -> m ()

-- symbols
class (Monad m) => MonadSym m where
  intern :: (T.Text -> m (Object m)) -> T.Text -> m (Object m)
  gensym :: (T.Text -> m (Object m)) -> m (Object m)

data Object m = Object (ObjID m) (ObjKind m)

type ObjRef m = Ref m (Object m)

data ObjKind m
  = Undef_
  | Empty_
  | Bool_ Bool
  | Num_ Integer
  | Str_ T.Text
  | Sym_ T.Text
  | Port_ Handle
  | Cons_ (ObjRef m) (ObjRef m)
  | Syn_ ([Object m] -> m (Object m))
  | Func_ ([Object m] -> m (Object m))
  | Cont_ [DWAction m] (Object m -> m (Object m))

data DWAction m = DWAction
  { dwid :: ObjID m,
    dwpre :: m (),
    dwpost :: m ()
  }

-- object constructors

newObject :: (MonadIDSupply m) => ObjKind m -> m (Object m)
newObject kind = flip Object kind <$> freshID

undef, empty, true, false :: forall m. (MonadIDSupply m) => Object m
undef = Object (undefID @m) Undef_
empty = Object (emptyID @m) Empty_
true = Object (trueID @m) (Bool_ True)
false = Object (falseID @m) (Bool_ False)

num :: (MonadIDSupply m) => Integer -> m (Object m)
num n = newObject (Num_ n)

str :: (MonadIDSupply m) => T.Text -> m (Object m)
str s = newObject (Str_ s)

sym :: (MonadIDSupply m, MonadSym m) => T.Text -> m (Object m)
sym s = intern (newObject . Sym_) s

freshSym :: (MonadIDSupply m, MonadSym m) => m (Object m)
freshSym = gensym (newObject . Sym_)

port :: (MonadIDSupply m) => Handle -> m (Object m)
port p = newObject (Port_ p)

cons :: (MonadIDSupply m, MonadRef m) => Object m -> Object m -> m (Object m)
cons car cdr = do
  car' <- newRef car
  cdr' <- newRef cdr
  newObject $ Cons_ car' cdr'

list :: (MonadIDSupply m, MonadRef m) => [Object m] -> m (Object m)
list = foldrM cons empty

dlist :: (MonadIDSupply m, MonadRef m) => NE.NonEmpty (Object m) -> Object m -> m (Object m)
dlist objs obj = foldrM cons obj objs

syn :: (MonadIDSupply m) => ([Object m] -> m (Object m)) -> m (Object m)
syn f = newObject (Syn_ f)

func :: (MonadIDSupply m) => ([Object m] -> m (Object m)) -> m (Object m)
func f = newObject (Func_ f)

cont :: (MonadIDSupply m) => [DWAction m] -> (Object m -> m (Object m)) -> m (Object m)
cont as k = newObject (Cont_ as k)

-- object destrcutors

objID :: Object m -> ObjID m
objID (Object x _) = x

pattern Undef :: Object m
pattern Undef <- Object _ Undef_

pattern Empty :: Object m
pattern Empty <- Object _ Empty_

pattern Bool :: Bool -> Object m
pattern Bool b <- Object _ (Bool_ b)

pattern Num :: Integer -> Object m
pattern Num n <- Object _ (Num_ n)

pattern Str :: T.Text -> Object m
pattern Str s <- Object _ (Str_ s)

pattern Sym :: T.Text -> Object m
pattern Sym s <- Object _ (Sym_ s)

pattern Port :: Handle -> Object m
pattern Port h <- Object _ (Port_ h)

pattern Cons :: ObjRef m -> ObjRef m -> Object m
pattern Cons r1 r2 <- Object _ (Cons_ r1 r2)

pattern Syn :: ([Object m] -> m (Object m)) -> Object m
pattern Syn f <- Object _ (Syn_ f)

pattern Func :: ([Object m] -> m (Object m)) -> Object m
pattern Func f <- Object _ (Func_ f)

pattern Cont :: [DWAction m] -> (Object m -> m (Object m)) -> Object m
pattern Cont as k <- Object _ (Cont_ as k)

{-# COMPLETE Undef, Empty, Bool, Num, Str, Sym, Port, Cons, Syn, Func, Cont #-}

data ListifyResult a
  = NonList a
  | List [a]
  | DList (NE.NonEmpty a) a
  deriving (Functor, Foldable, Traversable)

listify :: (MonadRef m) => Object m -> m (ListifyResult (Object m))
listify Empty = pure $ List []
listify (Cons ref1 ref2) = do
  obj <- deref ref1
  res <- listify =<< deref ref2
  case res of
    NonList obj' -> pure $ DList (NE.singleton obj) obj'
    List objs -> pure $ List (obj : objs)
    DList objs obj' -> pure $ DList (NE.cons obj objs) obj'
listify obj = pure $ NonList obj

unlistify :: (MonadIDSupply m, MonadRef m) => ListifyResult (Object m) -> m (Object m)
unlistify (NonList a) = pure a
unlistify (List as) = list as
unlistify (DList as a) = dlist as a
