{-# LANGUAGE BlockArguments #-}

module Language.Hasche.Eval
  ( EvalT,
    runEvalT,
    Env,
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
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Data.Unique
import Language.Hasche.Error
import Language.Hasche.Object
import System.IO.Unsafe
import Prelude hiding (lookup)

newtype EvalT m a = EvalT (ReaderT (Env m) (ContT (Object (EvalT m)) m) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFail,
      MonadReader (Env m),
      MonadCont,
      MonadThrow
    )

runEvalT :: (MonadIO m, MonadCatch m) => EvalT m (Object (EvalT m)) -> m (Either Error (Object (EvalT m)))
runEvalT (EvalT m) = do
  env <- emptyEnv
  m
    & flip runReaderT env
    & flip runContT pure
    & fmap Right
    & flip catch (pure . Left)

instance MonadIO m => MonadIDSupply (EvalT m) where
  type ObjID (EvalT m) = Unique
  freshID = liftIO newUnique
  undefID = unsafePerformIO newUnique
  emptyID = unsafePerformIO newUnique
  trueID = unsafePerformIO newUnique
  falseID = unsafePerformIO newUnique
  {-# NOINLINE undefID #-}
  {-# NOINLINE emptyID #-}
  {-# NOINLINE trueID #-}
  {-# NOINLINE falseID #-}

instance MonadIO m => MonadRef (EvalT m) where
  type Ref (EvalT m) = IORef
  newRef x = liftIO $ newIORef x
  deref r = liftIO $ readIORef r
  setRef r x = liftIO $ writeIORef r x

instance MonadIO m => MonadSym (EvalT m) where
  intern obj@(Sym s) = do
    table <- asks symbols
    liftIO $ HT.mutate table s \entry ->
      let obj' = fromMaybe obj entry in (Just obj', obj')
  intern _ = error "bug"

  gensym = do
    uniq <- hashUnique <$> liftIO newUnique
    sym $ "#G" <> T.pack (show uniq)

-- environment

data Env m = Env
  { binds :: NE.NonEmpty (HT.BasicHashTable T.Text (Ref (EvalT m) (Object (EvalT m)))),
    symbols :: HT.BasicHashTable T.Text (Object (EvalT m))
  }

emptyEnv :: MonadIO m => m (Env n)
emptyEnv = liftIO $ liftA2 Env (NE.singleton <$> HT.new) HT.new

withToplevelEnv :: EvalT m a -> EvalT m a
withToplevelEnv m = do
  flip local m \env -> env {binds = NE.singleton . NE.last $ binds env}

withNewScope :: MonadIO m => EvalT m a -> EvalT m a
withNewScope m = do
  b <- liftIO HT.new
  flip local m \env -> env {binds = NE.cons b (binds env)}

lookup' :: (MonadIO m, MonadThrow m) => T.Text -> EvalT m (Ref (EvalT m) (Object (EvalT m)))
lookup' k = do
  bs <- asks binds
  result <- liftIO $ asum <$> traverse (`HT.lookup` k) bs
  maybe (throw $ EUnboundVar k) pure result

lookup :: (MonadIO m, MonadThrow m) => T.Text -> EvalT m (Object (EvalT m))
lookup = lookup' >=> deref

set :: (MonadIO m, MonadThrow m) => T.Text -> Object (EvalT m) -> EvalT m ()
set k v = lookup' k >>= flip setRef v

bind :: MonadIO m => T.Text -> Object (EvalT m) -> EvalT m ()
bind k v = do
  b <- asks $ NE.head . binds
  liftIO . HT.insert b k =<< newRef v

-- like func but the body is evaluated under the current environment
closure ::
  MonadIO m =>
  ([Object (EvalT m)] -> EvalT m (Object (EvalT m))) ->
  EvalT m (Object (EvalT m))
closure f = do
  bs <- asks binds
  func $ local (\env -> env {binds = bs}) . withNewScope . f

-- evaluation

evalMany :: (MonadIO m, MonadThrow m) => [Object (EvalT m)] -> EvalT m (Object (EvalT m))
evalMany = traverseAndLast eval undef

eval :: (MonadIO m, MonadThrow m) => Object (EvalT m) -> EvalT m (Object (EvalT m))
eval code =
  listify code >>= \case
    NonList (Sym s) -> lookup s
    NonList obj' -> pure obj'
    List [] -> pure empty
    List (o : os) -> apply o os
    DList _ _ -> throw EInproperList

apply ::
  (MonadIO m, MonadThrow m) =>
  Object (EvalT m) ->
  [Object (EvalT m)] ->
  EvalT m (Object (EvalT m))
apply obj objs =
  eval obj >>= \case
    Syn s -> s objs
    Func f -> traverse eval objs >>= f
    Cont k -> eval (fromMaybe undef $ listToMaybe objs) >>= k
    _ -> throw EInvalidApplication
