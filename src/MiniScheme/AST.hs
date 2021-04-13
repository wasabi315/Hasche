{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.AST where

import Data.Text (Text)
import Data.Text qualified as Text

data Exp
  = Const Const
  | Id Id
  | App Exp [Exp]
  deriving (Show)

type Id = Text

type Const = Integer
