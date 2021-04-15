{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module MiniScheme.Driver
  ( runInterpreter,
    Error,
  )
where

import Control.Exception.Safe
import Data.Text (Text)
import MiniScheme.Interpreter qualified as MS
import MiniScheme.Parser qualified as MS

data Error
  = ParseError MS.ParseError
  | EvalError MS.EvalError

instance Show Error where
  show (ParseError err) = show err
  show (EvalError err) = show err

instance Exception Error

runInterpreter :: Text -> IO (Either Error MS.Value)
runInterpreter txt =
  case MS.parseExp txt of
    Left err -> pure (Left (ParseError err))
    Right e ->
      MS.interpret e >>= \case
        Right v -> pure (Right v)
        Left err -> pure (Left (EvalError err))