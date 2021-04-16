{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.AST
  ( Prog (..),
    Def (..),
    Exp (..),
    Atom (..),
    Id,
  )
where

import Data.Text (Text)

data Prog
  = Exp Exp
  | Def Def
  deriving (Show)

data Def
  = Const Id Exp
  deriving (Show)

data Exp
  = Atom Atom
  | App Exp [Exp]
  deriving (Show)

data Atom
  = Bool Bool
  | Int Integer
  | Str Text
  | Id Id
  deriving (Show)

type Id = Text
