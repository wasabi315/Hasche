{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Hasche.SExpr
  ( SExpr (..),
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
