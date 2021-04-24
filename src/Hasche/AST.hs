{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

module Hasche.AST
  ( Expr (..),
    Atom (..),
  )
where

import Data.Text (Text)

data Expr
  = Atom Atom
  | Pair Expr Expr
  deriving (Show)

infixr 5 `Pair`

data Atom
  = Empty
  | Bool Bool
  | Num Integer
  | Str Text
  | Ident Text
  deriving (Show)
