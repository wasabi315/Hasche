{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Interpreter where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import MiniScheme.AST qualified as AST

eval :: AST.Exp -> Either String Value
eval (AST.Atom a) = evalAtom a
eval (AST.App e es) = do
  func <- eval e >>= expectProc
  args <- traverse eval es
  func args

type Env = HashMap AST.Id Value

data Value
  = Int Integer
  | Bool Bool
  | Proc ([Value] -> Either String Value)

instance Show Value where
  show (Int n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show (Proc _) = "<procedure>"

evalAtom :: AST.Atom -> Either String Value
evalAtom (AST.Int n) = Right (Int n)
evalAtom (AST.Bool b) = Right (Bool b)
evalAtom (AST.Id i) =
  case HM.lookup i defaultEnv of
    Just v -> Right v
    Nothing -> Left "unknown id"

expectInt :: Value -> Either String Integer
expectInt (Int n) = Right n
expectInt _ = Left "expect number"

expectBool :: Value -> Either String Bool
expectBool (Bool b) = Right b
expectBool _ = Left "expect boolean"

expectProc :: Value -> Either String ([Value] -> Either String Value)
expectProc (Proc f) = Right f
expectProc _ = Left "expect procedure"

defaultEnv :: Env
defaultEnv =
  HM.fromList
    [ ( "number?",
        Proc \case
          [Int _] -> Right (Bool True)
          [_] -> Right (Bool False)
          _ -> Left "illegal number of arguments"
      ),
      ( "boolean?",
        Proc \case
          [Bool _] -> Right (Bool True)
          [_] -> Right (Bool False)
          _ -> Left "illegal number of arguments"
      ),
      ( "procedure?",
        Proc \case
          [Proc _] -> Right (Bool True)
          [_] -> Right (Bool False)
          _ -> Left "illegal number of arguments"
      ),
      ( "+",
        Proc (fmap (Int . sum) . traverse expectInt)
      ),
      ( "-",
        Proc $
          traverse expectInt >=> \case
            [] -> Left "expect at least one number"
            n : ns -> Right $ Int (n - sum ns)
      ),
      ( "*",
        Proc (fmap (Int . product) . traverse expectInt)
      ),
      ( "/",
        Proc $
          traverse expectInt >=> \case
            [] -> Left "expect at least one number"
            n : ns -> Right $ Int (n `div` product ns)
      ),
      ( "=",
        Proc $
          traverse expectInt >=> \case
            [n1, n2] -> Right $ Bool (n1 == n2)
            _ -> Left "illegal number of arguments"
      ),
      ( ">",
        Proc $
          traverse expectInt >=> \case
            [n1, n2] -> Right $ Bool (n1 > n2)
            _ -> Left "illegal number of arguments"
      ),
      ( ">=",
        Proc $
          traverse expectInt >=> \case
            [n1, n2] -> Right $ Bool (n1 >= n2)
            _ -> Left "illegal number of arguments"
      ),
      ( "<",
        Proc $
          traverse expectInt >=> \case
            [n1, n2] -> Right $ Bool (n1 < n2)
            _ -> Left "illegal number of arguments"
      ),
      ( "<=",
        Proc $
          traverse expectInt >=> \case
            [n1, n2] -> Right $ Bool (n1 <= n2)
            _ -> Left "illegal number of arguments"
      ),
      ( "not",
        Proc \case
          [v] -> expectBool v >>= Right . Bool . not
          _ -> Left "illegal number of arguments"
      )
    ]
