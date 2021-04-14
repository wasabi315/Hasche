{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.AST where

import Data.Text (Text)

data Exp
  = Atom Atom
  | App Exp [Exp]
  deriving (Show)

data Atom
  = Bool Bool
  | Int Integer
  | Id Id
  deriving (Show)

type Id = Text
