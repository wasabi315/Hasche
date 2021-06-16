{-# LANGUAGE ImportQualifiedPost #-}

module Language.Hasche.Eval.Error where

import Control.Exception.Safe
import Data.Text (Text)
import Data.Text qualified as T
import Language.Hasche.Syntax.Reader

-- Errors

data Error
  = ReadError ReadError
  | FileError IOException
  | SynError Text
  | EvalError Text
  | UserError Text
  deriving (Show)

instance Exception Error where
  displayException (ReadError e) = "[READ ERROR]: " ++ displayException e
  displayException (FileError e) = "[FILE ERROR]:" ++ displayException e
  displayException (SynError e) = "[SYNTAX ERROR]: " ++ T.unpack e
  displayException (EvalError e) = "[EVAL ERROR]: " ++ T.unpack e
  displayException (UserError e) = "[USER ERROR]: " ++ T.unpack e
