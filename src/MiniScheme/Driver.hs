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

newInterpreter :: IO (Text -> IO (Either Error Value))
newInterpreter = do
  eval <- newEvaluator

  pure \txt ->
    case parseProg txt of
      Left err -> pure (Left (ParseError err))
      Right prog -> first EvalError <$> eval prog
