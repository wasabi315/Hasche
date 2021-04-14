{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Interpreter where

import Control.Monad
import MiniScheme.AST qualified as AST

eval :: AST.Exp -> Either String Value
eval = \case
  AST.Atom a -> evalAtom a
  AST.App e es -> do
    func <- eval e
    args <- traverse eval es
    case func of
      Proc f -> f args
      _ -> Left "Cannot apply to non procedure value"

data Value
  = Int Integer
  | Bool Bool
  | Proc ([Value] -> Either String Value)

instance Show Value where
  show (Int n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show (Proc _) = "<procedure>"

evalAtom :: AST.Atom -> Either String Value
evalAtom = \case
  AST.Int n ->
    Right (Int n)
  AST.Bool b ->
    Right (Bool b)
  AST.Id "number?" ->
    Right $ Proc \case
      [Int _] -> Right (Bool True)
      [_] -> Right (Bool False)
      _ -> Left "illegal number of arguments"
  AST.Id "boolean?" ->
    Right $ Proc \case
      [Bool _] -> Right (Bool True)
      [_] -> Right (Bool False)
      _ -> Left "illegal number of arguments"
  AST.Id "+" ->
    Right $ Proc (fmap (Int . sum) . traverse expectInt)
  AST.Id "-" ->
    Right . Proc $
      traverse expectInt >=> \case
        [] -> Left "expect at least one number"
        n : ns -> Right $ Int (n - sum ns)
  AST.Id "*" ->
    Right $ Proc (fmap (Int . product) . traverse expectInt)
  AST.Id "/" ->
    Right . Proc $
      traverse expectInt >=> \case
        [] -> Left "expect at least one number"
        n : ns -> Right $ Int (n `div` product ns)
  AST.Id "=" ->
    Right . Proc $
      traverse expectInt >=> \case
        [n1, n2] -> Right $ Bool (n1 == n2)
        _ -> Left "illegal number of arguments"
  AST.Id ">" ->
    Right . Proc $
      traverse expectInt >=> \case
        [n1, n2] -> Right $ Bool (n1 > n2)
        _ -> Left "illegal number of arguments"
  AST.Id ">=" ->
    Right . Proc $
      traverse expectInt >=> \case
        [n1, n2] -> Right $ Bool (n1 >= n2)
        _ -> Left "illegal number of arguments"
  AST.Id "<" ->
    Right . Proc $
      traverse expectInt >=> \case
        [n1, n2] -> Right $ Bool (n1 < n2)
        _ -> Left "illegal number of arguments"
  AST.Id "<=" ->
    Right . Proc $
      traverse expectInt >=> \case
        [n1, n2] -> Right $ Bool (n1 <= n2)
        _ -> Left "illegal number of arguments"
  AST.Id "not" ->
    Right $ Proc \case
      [v] -> expectBool v >>= Right . Bool . not
      _ -> Left "illegal number of arguments"
  _ ->
    Left "unexpected atom"

expectInt :: Value -> Either String Integer
expectInt (Int n) = Right n
expectInt _ = Left "expect number"

expectBool :: Value -> Either String Bool
expectBool (Bool b) = Right b
expectBool _ = Left "expect boolean"
