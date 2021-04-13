{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.AST where

import Data.Text (Text)

data Exp
  = Const Const
  | Id Id
  | App Exp [Exp]
  deriving (Show)

type Id = Text

-- type Const = Integer
data Const
  = Bool Bool
  | Int Integer
  deriving (Eq)

instance Show Const where
  show (Bool b) = if b then "#t" else "#f"
  show (Int n) = show n
