{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Interpreter where

import MiniScheme.AST qualified as AST

interpret :: AST.Exp -> Maybe Integer
interpret = \case
  AST.Const n ->
    Just n
  AST.App (AST.Id "+") es ->
    sum <$> traverse interpret es
  AST.App (AST.Id "-") (e : es) ->
    (-) <$> interpret e <*> fmap sum (traverse interpret es)
  AST.App (AST.Id "*") es ->
    product <$> traverse interpret es
  AST.App (AST.Id "/") (e : es) ->
    div <$> interpret e <*> fmap product (traverse interpret es)
  _ ->
    Nothing
