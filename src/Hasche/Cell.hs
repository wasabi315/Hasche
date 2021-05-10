module Hasche.Cell
  ( Cell,
    newCell,
    deref,
    (.=),
  )
where

import Control.Monad.IO.Class
import Data.IORef

-- Mutable cell
-- For variables and cons cells

type Cell = IORef

newCell :: MonadIO m => a -> m (Cell a)
newCell = liftIO . newIORef

deref :: MonadIO m => Cell a -> m a
deref = liftIO . readIORef

(.=) :: MonadIO m => Cell a -> a -> m ()
r .= x = liftIO $ modifyIORef' r (const x)

infix 0 .=
