{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Hasche.Types
  ( Eval,
    runEval,
    Env,
    emptyEnv,
    withNewScope,
    findObj,
    bindObj,
    Error (..),
    Object (..),
    ObjID,
    ObjRef,
    ObjKind,
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
    oid,
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
    deref,
    setRef,
  )
where

import Control.Applicative (asum, liftA2)
import Control.Exception.Safe (Exception (displayException), MonadThrow, catch)
import Control.Monad.Cont (ContT (..), MonadCont)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, local)
import Data.HashTable.IO qualified as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Unique (Unique, newUnique)
import GHC.IO (unsafePerformIO)
import System.IO (Handle)

-- evaluator

newtype Eval a = Eval (ReaderT Env (ContT Object IO) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadCont,
      MonadThrow
    )

runEval :: Env -> Eval Object -> IO (Either Error Object)
runEval env (Eval m) =
  flip catch (pure . Left)
    . fmap Right
    . flip runContT pure
    . flip runReaderT env
    $ m

data Error = forall e. Exception e => Error e

deriving instance Show Error

instance Exception Error where
  displayException (Error e) = displayException e

-- environment

data Env = Env
  { binds :: NE.NonEmpty (HT.BasicHashTable T.Text Object),
    symbols :: HT.BasicHashTable T.Text Object
  }

emptyEnv :: MonadIO m => m Env
emptyEnv = liftIO $ liftA2 Env (NE.singleton <$> HT.new) HT.new

withNewScope :: Eval a -> Eval a
withNewScope m = do
  bind <- liftIO HT.new
  flip local m \env -> env {binds = NE.cons bind env.binds}

findObj :: T.Text -> Eval (Maybe Object)
findObj k = do
  binds <- asks binds
  liftIO $ asum <$> traverse (`HT.lookup` k) binds

bindObj :: T.Text -> Object -> Eval ()
bindObj k o = do
  binds <- asks binds
  liftIO $ HT.insert (NE.head binds) k o

-- object

data Object = Object ObjID ObjKind

newtype ObjRef = ObjRef (IORef Object)
  deriving stock (Eq)

newtype ObjID = ObjID Unique
  deriving stock (Eq)

data ObjKind
  = Undef_
  | Empty_
  | Bool_ Bool
  | Num_ Integer
  | Str_ T.Text
  | Sym_ T.Text
  | Port_ Handle
  | Cons_ ObjRef ObjRef
  | Syn_ (Env -> Object -> Eval Object)
  | Func_ ([Object] -> Eval Object)
  | Cont_ ([Object] -> Eval Object)

-- object constructors

newObject :: MonadIO m => ObjKind -> m Object
newObject kind = flip Object kind . ObjID <$> liftIO newUnique

undef, empty, true, false :: Object
undef = unsafePerformIO $ newObject Undef_
empty = unsafePerformIO $ newObject Empty_
true = unsafePerformIO $ newObject (Bool_ True)
false = unsafePerformIO $ newObject (Bool_ False)
{-# NOINLINE undef #-}
{-# NOINLINE empty #-}
{-# NOINLINE true #-}
{-# NOINLINE false #-}

num :: Integer -> Eval Object
num = newObject . Num_

str :: T.Text -> Eval Object
str = newObject . Str_

sym :: T.Text -> Eval Object
sym s = do
  symbols <- asks symbols
  liftIO $ HT.mutateIO symbols s \entry -> do
    obj <- maybe (newObject $ Sym_ s) pure entry
    pure (Just obj, obj)

port :: Handle -> Eval Object
port = newObject . Port_

cons :: Object -> Object -> Eval Object
cons car cdr = liftIO do
  car' <- newIORef car
  cdr' <- newIORef cdr
  newObject $ Cons_ (ObjRef car') (ObjRef cdr')

syn :: (Env -> Object -> Eval Object) -> Eval Object
syn = newObject . Syn_

func :: ([Object] -> Eval Object) -> Eval Object
func = newObject . Func_

cont :: ([Object] -> Eval Object) -> Eval Object
cont = newObject . Cont_

-- object destrcutors

oid :: Object -> ObjID
oid (Object x _) = x

pattern Undef :: Object
pattern Undef <- Object _ Undef_

pattern Empty :: Object
pattern Empty <- Object _ Empty_

pattern Bool :: Bool -> Object
pattern Bool b <- Object _ (Bool_ b)

pattern Num :: Integer -> Object
pattern Num n <- Object _ (Num_ n)

pattern Str :: T.Text -> Object
pattern Str s <- Object _ (Str_ s)

pattern Sym :: T.Text -> Object
pattern Sym s <- Object _ (Sym_ s)

pattern Port :: Handle -> Object
pattern Port h <- Object _ (Port_ h)

pattern Cons :: ObjRef -> ObjRef -> Object
pattern Cons r1 r2 <- Object _ (Cons_ r1 r2)

pattern Syn :: (Env -> Object -> Eval Object) -> Object
pattern Syn f <- Object _ (Syn_ f)

pattern Func :: ([Object] -> Eval Object) -> Object
pattern Func f <- Object _ (Func_ f)

pattern Cont :: ([Object] -> Eval Object) -> Object
pattern Cont k <- Object _ (Cont_ k)

{-# COMPLETE Undef, Empty, Bool, Num, Str, Sym, Port, Cons, Syn, Func, Cont #-}

-- mutable reference

deref :: ObjRef -> Eval Object
deref (ObjRef r) = liftIO $ readIORef r

setRef :: ObjRef -> Object -> Eval ()
setRef (ObjRef r) o = liftIO $ writeIORef r o
