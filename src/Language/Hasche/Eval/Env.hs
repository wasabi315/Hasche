{-# LANGUAGE StrictData #-}

module Language.Hasche.Eval.Env
  ( Env,
    empty,
    child,
    lookup,
    bind,
  )
where

import Control.Applicative (asum)
import Control.Monad.IO.Class
import Data.Cell (Cell)
import Data.Cell qualified as Cell
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.Hashable
import Data.List.NonEmpty qualified as NL
import Prelude hiding (lookup)

newtype Env k v = Env (NL.NonEmpty (BasicHashTable k (Cell v)))

empty :: MonadIO m => m (Env k v)
empty = Env . NL.singleton <$> liftIO HT.new

child :: MonadIO m => Env k v -> m (Env k v)
child (Env envs) = Env . (`NL.cons` envs) <$> liftIO HT.new

lookup :: (MonadIO m, Hashable k) => Env k v -> k -> m (Maybe (Cell v))
lookup (Env envs) k = liftIO $ asum <$> traverse (`HT.lookup` k) envs

bind :: (MonadIO m, Hashable k) => Env k v -> k -> v -> m ()
bind (Env envs) k v = liftIO $ Cell.new v >>= HT.insert (NL.head envs) k
