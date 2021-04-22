{-# LANGUAGE StrictData #-}

module MiniScheme.AST
  ( Prog (..),
    Def (..),
    Body (..),
    Exp (..),
    Atom (..),
    Id,
    Number,
  )
where

import Data.List.NonEmpty (NonEmpty)
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
  | Lam [Id] Body
  | Set Id Exp
  | If Exp Exp (Maybe Exp)
  | Let (Maybe Id) [(Id, Exp)] Body
  | LetA (Maybe Id) [(Id, Exp)] Body
  | LetRec (Maybe Id) [(Id, Exp)] Body
  | Begin [Exp]
  | App Exp [Exp]
  deriving (Show)

data Body
  = Body [Def] (NonEmpty Exp)
  deriving (Show)

data Atom
  = Empty
  | Bool Bool
  | Num Number
  | Str Text
  | Id Id
  deriving (Show)

type Id = Text

type Number = Integer
