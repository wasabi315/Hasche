{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Interpreter where

import MiniScheme.AST qualified as AST
import MiniScheme.Value qualified as Value

eval :: AST.Exp -> Either String Value.Value
eval = \case
  AST.App e es -> do
    func <- Value.interpret e
    args <- traverse eval es
    case func of
      Value.Proc f -> f args
      _ -> Left "Cannot apply to non procedure value"
  e -> Value.interpret e
