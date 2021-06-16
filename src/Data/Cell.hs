module Data.Cell
  ( Cell,
    new,
    deref,
    set,
  )
where

import Control.Monad.IO.Class
import Data.IORef

-- Mutable cell

type Cell = IORef

new :: MonadIO m => a -> m (Cell a)
new = liftIO . newIORef

deref :: MonadIO m => Cell a -> m a
deref = liftIO . readIORef

set :: MonadIO m => Cell a -> a -> m ()
set r x = liftIO $ modifyIORef' r (const x)
