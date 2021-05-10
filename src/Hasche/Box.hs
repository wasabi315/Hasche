{-# LANGUAGE StrictData #-}

module Hasche.Box
  ( Box,
    Loc,
    alloc,
    loc,
    val,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Unique

-- Immutable box
-- Supports pseudo-pointer-equality for implementing scheme's eq? procedure.

data Box a = Box a Loc

type Loc = Unique

instance Eq (Box a) where
  Box _ x == Box _ y = x == y

alloc :: MonadIO m => a -> m (Box a)
alloc x = Box x <$!> liftIO newUnique

loc :: Box a -> Loc
loc (Box _ l) = l

val :: Box a -> a
val (Box x _) = x
