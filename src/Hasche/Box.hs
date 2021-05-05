{-# LANGUAGE StrictData #-}

module Hasche.Box
  ( Box,
    alloc,
    val,
  )
where

import Control.Monad
import Data.Unique

-- Immutable box
-- Supports pseudo-pointer-equality for implementing scheme's eq? procedure.

data Box a = Box a Unique

instance Eq (Box a) where
  Box _ x == Box _ y = x == y

alloc :: a -> IO (Box a)
alloc x = Box x <$!> newUnique

val :: Box a -> a
val (Box x _) = x
