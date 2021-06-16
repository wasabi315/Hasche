module Data.Box
  ( Box,
    Loc,
    loc,
    new,
    deref,
  )
where

import Control.Monad.IO.Class
import Data.Unique

-- Immutable box

data Box a = Box a !Loc

type Loc = Unique

-- Pseudo-pointer-equality
instance Eq (Box a) where
  Box _ x == Box _ y = x == y

new :: MonadIO m => a -> m (Box a)
new x = Box x <$> liftIO newUnique

loc :: Box a -> Loc
loc (Box _ l) = l

deref :: Box a -> a
deref (Box x _) = x
