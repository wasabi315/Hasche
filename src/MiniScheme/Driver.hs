{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module MiniScheme.Driver
  ( newInterpreter,
    pretty,
    Error,
  )
where

import Control.Exception.Safe
import Data.Bifunctor
import Data.Text (Text)
import MiniScheme.Evaluator
import MiniScheme.Parser

data Error
  = ParseError ParseError
  | EvalError EvalError

instance Show Error where
  show (ParseError err) = show err
  show (EvalError err) = show err

instance Exception Error

newInterpreter :: FilePath -> IO (Text -> IO (Either Error Value))
newInterpreter path = do
  eval <- newEvaluator

  pure \txt ->
    case parseProg path txt of
      Left err -> pure (Left (ParseError err))
      Right prog -> first EvalError <$> eval prog
