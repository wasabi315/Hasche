module Data.Foldable.Extra
  ( traverseAndLast,
  )
where

import Data.Foldable

traverseAndLast ::
  (Foldable t, Monad f) =>
  (a -> f b) ->
  b ->
  t a ->
  f b
traverseAndLast f = foldlM (const f)
{-# INLINE traverseAndLast #-}
