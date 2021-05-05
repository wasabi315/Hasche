{-# LANGUAGE StrictData #-}

module Hasche.Ptr
  ( Ptr,
    alloc,
    val,
  )
where

import Control.Monad
import Data.Unique

-- Pseudo pointer type for pointer-equality, which is needed by scheme's eq? procedure.

data Ptr a = Ptr a Unique

instance Eq (Ptr a) where
  Ptr _ x == Ptr _ y = x == y

alloc :: a -> IO (Ptr a)
alloc x = Ptr x <$!> newUnique

val :: Ptr a -> a
val (Ptr x _) = x
