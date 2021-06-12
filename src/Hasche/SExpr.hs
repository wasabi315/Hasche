{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Hasche.SExpr
  ( SExpr (..),
    pattern SList,
    pattern SDList,
    pattern SQuote,
    pattern SQQ,
    pattern SUQ,
    pattern SUQS,
  )
where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)

-- The S-Expression

data SExpr
  = SEmpty
  | SBool Bool
  | SNum Integer
  | SStr Text
  | SSym Text
  | SCons SExpr SExpr
  deriving (Show)

infixr 4 `SCons`

pattern SQuote :: SExpr -> SExpr
pattern SQuote e = SSym "quote" `SCons` e `SCons` SEmpty

pattern SQQ :: SExpr -> SExpr
pattern SQQ e = SSym "quasiquote" `SCons` e `SCons` SEmpty

pattern SUQ :: SExpr -> SExpr
pattern SUQ e = SSym "unquote" `SCons` e `SCons` SEmpty

pattern SUQS :: SExpr -> SExpr
pattern SUQS e = SSym "unquote-splicing" `SCons` e `SCons` SEmpty

toList :: SExpr -> Maybe (NonEmpty SExpr)
toList (e `SCons` SEmpty) = pure $ e NE.:| []
toList (e `SCons` es) = NE.cons e <$> toList es
toList _ = Nothing

pattern SList :: NonEmpty SExpr -> SExpr
pattern SList es <-
  (toList -> Just es)
  where
    SList es = foldr SCons SEmpty es

toList' :: SExpr -> Maybe (NonEmpty SExpr, SExpr)
toList' (e `SCons` es@SCons {}) = first (NE.cons e) <$> toList' es
toList' (_ `SCons` SEmpty) = Nothing
toList' (e1 `SCons` e2) = pure (e1 NE.:| [], e2)
toList' _ = Nothing

pattern SDList :: NonEmpty SExpr -> SExpr -> SExpr
pattern SDList es e <-
  (toList' -> Just (es, e))
  where
    SDList es e = foldr SCons e es

{-# COMPLETE SEmpty, SBool, SNum, SStr, SSym, SList, SDList #-}
