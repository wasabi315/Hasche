module Language.Hasche.Eval
  ( EvalT,
    runEvalT,
    Env,
    emptyEnv,
    withToplevelEnv,
    withNewScope,
    lookup,
    set,
    bind,
    closure,
    evalMany,
    eval,
    apply,
  )
where

import Control.Applicative hiding (empty)
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Foldable.Extra
import Data.Function
import Data.HashTable.IO qualified as HT
import Data.IORef
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Data.Unique
import Language.Hasche.Error
import Language.Hasche.Object
import System.IO.Unsafe
import Prelude hiding (lookup)

type EvalT :: Type -> (Type -> Type) -> Type -> Type
newtype EvalT r m a = EvalT (ReaderT (Env (EvalT r m)) (ContT r m) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFail,
      MonadReader (Env (EvalT r m)),
      MonadCont,
      MonadThrow
    )

runEvalT :: (MonadIO m) => Env (EvalT a m) -> EvalT a m a -> m a
runEvalT env (EvalT m) = do
  m
    & flip runReaderT env
    & flip runContT pure

instance (MonadIO m) => MonadIDSupply (EvalT r m) where
  type ObjID (EvalT r m) = Unique
  freshID = liftIO newUnique
  undefID = unsafePerformIO newUnique
  emptyID = unsafePerformIO newUnique
  trueID = unsafePerformIO newUnique
  falseID = unsafePerformIO newUnique
  {-# NOINLINE undefID #-}
  {-# NOINLINE emptyID #-}
  {-# NOINLINE trueID #-}
  {-# NOINLINE falseID #-}

instance (MonadIO m) => MonadRef (EvalT r m) where
  type Ref (EvalT r m) = IORef
  newRef x = liftIO $ newIORef x
  deref r = liftIO $ readIORef r
  setRef r x = liftIO $ writeIORef r x

instance (MonadIO m) => MonadSym (EvalT r m) where
  intern alloc s = do
    table <- asks symbols
    liftIO (HT.lookup table s) >>= \case
      Just obj -> pure obj
      Nothing -> do
        obj <- alloc s
        liftIO $ HT.insert table s obj
        pure obj

  gensym alloc = do
    uniq <- hashUnique <$> liftIO newUnique
    alloc $ "#G" <> T.pack (show uniq)

-- environment

data Env m = Env
  { binds :: NE.NonEmpty (HT.BasicHashTable T.Text (Ref m (Object m))),
    symbols :: HT.BasicHashTable T.Text (Object m)
  }

emptyEnv :: (MonadIO m) => m (Env n)
emptyEnv = liftIO $ liftA2 Env (NE.singleton <$> HT.new) HT.new

withToplevelEnv :: EvalT r m a -> EvalT r m a
withToplevelEnv m = do
  flip local m \env -> env {binds = NE.singleton . NE.last $ binds env}

withNewScope :: (MonadIO m) => EvalT r m a -> EvalT r m a
withNewScope m = do
  b <- liftIO HT.new
  flip local m \env -> env {binds = NE.cons b (binds env)}

lookup' :: (MonadIO m, MonadThrow m) => T.Text -> EvalT r m (Ref (EvalT r m) (Object (EvalT r m)))
lookup' k = do
  bs <- asks binds
  result <- liftIO $ asum <$> traverse (`HT.lookup` k) bs
  maybe (throw $ EUnboundVar k) pure result

lookup :: (MonadIO m, MonadThrow m) => T.Text -> EvalT r m (Object (EvalT r m))
lookup = lookup' >=> deref

set :: (MonadIO m, MonadThrow m) => T.Text -> Object (EvalT r m) -> EvalT r m ()
set k v = lookup' k >>= flip setRef v

bind :: (MonadIO m) => T.Text -> Object (EvalT r m) -> EvalT r m ()
bind k v = do
  b <- asks $ NE.head . binds
  liftIO . HT.insert b k =<< newRef v

-- like func but the body is evaluated under the current environment
closure ::
  (MonadIO m) =>
  ([Object (EvalT r m)] -> EvalT r m (Object (EvalT r m))) ->
  EvalT r m (Object (EvalT r m))
closure f = do
  bs <- asks binds
  func $ local (\env -> env {binds = bs}) . withNewScope . f

-- evaluation

evalMany :: (MonadIO m, MonadThrow m) => [Object (EvalT r m)] -> EvalT r m (Object (EvalT r m))
evalMany = traverseAndLast eval undef

eval :: (MonadIO m, MonadThrow m) => Object (EvalT r m) -> EvalT r m (Object (EvalT r m))
eval code =
  listify code >>= \case
    NonList (Sym s) -> lookup s
    NonList obj' -> pure obj'
    List [] -> pure empty
    List (expr : exprs) -> do
      eval expr >>= \case
        Syn s -> s exprs
        obj -> traverse eval exprs >>= apply obj
    DList _ _ -> throw EInproperList

apply ::
  (MonadIO m, MonadThrow m) =>
  Object (EvalT r m) ->
  [Object (EvalT r m)] ->
  EvalT r m (Object (EvalT r m))
apply obj objs = case obj of
  Func f -> f objs
  Cont k -> k $ fromMaybe undef (listToMaybe objs)
  _ -> throw EInvalidApplication
