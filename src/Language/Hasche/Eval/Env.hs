{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Hasche.Eval.Env
  ( Env,
    empty,
    child,
    lookup,
    bind,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Cell (Cell)
import Data.Cell qualified as Cell
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.Hashable
import Prelude hiding (lookup)

data Env k v = Env
  { binds :: BasicHashTable k (Cell v),
    parent :: Maybe (Env k v)
  }

empty :: MonadIO m => m (Env k v)
empty = flip Env Nothing <$> liftIO HT.new

child :: MonadIO m => Env k v -> m (Env k v)
child env = flip Env (Just env) <$> liftIO HT.new

lookup :: (MonadIO m, Eq k, Hashable k) => Env k v -> k -> m (Maybe (Cell v))
lookup e k = liftIO $ lookup' e
  where
    lookup' Env {..} =
      HT.lookup binds k >>= \case
        Just v -> pure (Just v)
        Nothing -> case parent of
          Just env' -> lookup' env'
          Nothing -> pure Nothing

bind :: (MonadIO m, Eq k, Hashable k) => Env k v -> k -> v -> m ()
bind e k v = liftIO $ Cell.new v >>= HT.insert (binds e) k
