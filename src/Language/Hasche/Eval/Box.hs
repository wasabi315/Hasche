module Language.Hasche.Eval.Box
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
-- Supports pseudo-pointer-equality for implementing scheme's eq? procedure.

data Box a = Box a !Loc

type Loc = Unique

instance Eq (Box a) where
  Box _ x == Box _ y = x == y

new :: MonadIO m => a -> m (Box a)
new x = Box x <$> liftIO newUnique

loc :: Box a -> Loc
loc (Box _ l) = l

deref :: Box a -> a
deref (Box x _) = x
