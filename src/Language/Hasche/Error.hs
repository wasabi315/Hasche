module Language.Hasche.Error (Error (..)) where

import Control.Exception.Safe
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec

data Error
  = EUnboundVar T.Text
  | EInproperList
  | EArityMismatch
  | EInvalidApplication
  | ERead (ParseErrorBundle T.Text Void)
  | EType
  | EDividedByZero
  | EUser T.Text
  | EIO IOException
  | ESyntax
  deriving stock (Show)

instance Exception Error where
  displayException = \case
    EUnboundVar v -> "unbound variable " ++ show v
    EInproperList -> "proper list required"
    EArityMismatch -> "arity mismatch"
    EInvalidApplication -> "invalid application"
    ERead err -> displayException err
    EType -> "type error"
    EDividedByZero -> "divided by 0"
    EUser err -> "user error: " ++ show err
    EIO err -> "IO error: " ++ displayException err
    ESyntax -> "syntax error"
