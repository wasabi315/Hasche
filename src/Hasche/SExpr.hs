{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Hasche.SExpr
  ( SExpr (..),
    pattern SQuote,
    pattern SQQ,
    pattern SUQ,
    pattern SUQS,
  )
where

import Data.Text (Text)

-- The S-Expression

data SExpr
  = SBool Bool
  | SNum Integer
  | SStr Text
  | SSym Text
  | SList [SExpr] (Maybe SExpr)
  deriving (Show)

pattern SQuote :: SExpr -> SExpr
pattern SQuote e = SList [SSym "quote", e] Nothing

pattern SQQ :: SExpr -> SExpr
pattern SQQ e = SList [SSym "quasiquote", e] Nothing

pattern SUQ :: SExpr -> SExpr
pattern SUQ e = SList [SSym "unquote", e] Nothing

pattern SUQS :: SExpr -> SExpr
pattern SUQS e = SList [SSym "unquote-splicing", e] Nothing
