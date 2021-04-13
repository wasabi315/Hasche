{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Interpreter where

import MiniScheme.AST qualified as AST

interpret :: AST.Exp -> Either String Integer
interpret = \case
  AST.Const n ->
    Right n
  AST.App (AST.Id "+") es ->
    sum <$> traverse interpret es
  AST.App (AST.Id "-") [] ->
    Left "expect at least one number"
  AST.App (AST.Id "-") (e : es) ->
    (-) <$> interpret e <*> fmap sum (traverse interpret es)
  AST.App (AST.Id "*") es ->
    product <$> traverse interpret es
  AST.App (AST.Id "/") [] ->
    Left "expect at least one number"
  AST.App (AST.Id "/") (e : es) ->
    div <$> interpret e <*> fmap product (traverse interpret es)
  e ->
    Left $ "illegal expression: " ++ show e
