{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Interpreter where

import Control.Monad
import MiniScheme.AST qualified as AST

interpret :: AST.Exp -> Either String AST.Const
interpret = \case
  AST.Const n ->
    Right n
  AST.App (AST.Id "number?") [e] ->
    interpret e >>= \case
      AST.Int _ -> Right (AST.Bool True)
      _ -> Right (AST.Bool False)
  AST.App (AST.Id "+") es ->
    AST.Int . sum <$> traverse (interpret >=> expectInt) es
  AST.App (AST.Id "-") [] ->
    Left "expect at least one number"
  AST.App (AST.Id "-") (e : es) ->
    (AST.Int .) . (-)
      <$> (interpret >=> expectInt) e
      <*> fmap sum (traverse (interpret >=> expectInt) es)
  AST.App (AST.Id "*") es ->
    AST.Int . product <$> traverse (interpret >=> expectInt) es
  AST.App (AST.Id "/") [] ->
    Left "expect at least one number"
  AST.App (AST.Id "/") (e : es) ->
    (AST.Int .) . div
      <$> (interpret >=> expectInt) e
      <*> fmap sum (traverse (interpret >=> expectInt) es)
  AST.App (AST.Id "boolean?") [e] ->
    interpret e >>= \case
      AST.Bool _ -> Right (AST.Bool True)
      _ -> Right (AST.Bool False)
  AST.App (AST.Id "=") [e1, e2] -> do
    n1 <- interpret e1 >>= expectInt
    n2 <- interpret e2 >>= expectInt
    pure . AST.Bool $! n1 == n2
  AST.App (AST.Id "=") es ->
    Left $ "=: expect 2 arguments but got " ++ show (length es)
  AST.App (AST.Id "<") [e1, e2] -> do
    n1 <- interpret e1 >>= expectInt
    n2 <- interpret e2 >>= expectInt
    pure . AST.Bool $! n1 < n2
  AST.App (AST.Id "<") es ->
    Left $ "<: expect 2 arguments but got " ++ show (length es)
  AST.App (AST.Id ">") [e1, e2] -> do
    n1 <- interpret e1 >>= expectInt
    n2 <- interpret e2 >>= expectInt
    pure . AST.Bool $! n1 > n2
  AST.App (AST.Id ">") es ->
    Left $ ">: expect 2 arguments but got " ++ show (length es)
  AST.App (AST.Id "<=") [e1, e2] -> do
    n1 <- interpret e1 >>= expectInt
    n2 <- interpret e2 >>= expectInt
    pure . AST.Bool $! n1 <= n2
  AST.App (AST.Id "<=") es ->
    Left $ "<=: expect 2 arguments but got " ++ show (length es)
  AST.App (AST.Id ">=") [e1, e2] -> do
    n1 <- interpret e1 >>= expectInt
    n2 <- interpret e2 >>= expectInt
    pure . AST.Bool $! n1 >= n2
  AST.App (AST.Id ">=") es ->
    Left $ ">=: expect 2 arguments but got " ++ show (length es)
  e ->
    Left $ "illegal expression: " ++ show e

expectInt :: AST.Const -> Either String Integer
expectInt (AST.Bool _) = Left "expect number but got boolean"
expectInt (AST.Int n) = Right n

expectBool :: AST.Const -> Either String Bool
expectBool (AST.Bool b) = Right b
expectBool (AST.Int _) = Left "expect bool but got number"
