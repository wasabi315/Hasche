{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module MiniScheme.Interpreter
  ( interpret,
    Value,
    EvalError,
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.Text (Text)
import Data.Text qualified as Text
import MiniScheme.AST qualified as AST

interpret :: AST.Exp -> IO (Either EvalError Value)
interpret e =
  catch
    (Right . Value <$> runInterpreter (eval e))
    (pure . Left)

newtype EvalError = EvalError Text

instance Show EvalError where
  show (EvalError reason) = Text.unpack reason

instance Exception EvalError

type Interpreter = IO

runInterpreter :: Interpreter a -> IO a
runInterpreter = id

data Value = forall m. Value (Value' m)

instance Show Value where
  show (Value v) = show v

data Value' m
  = Int Integer
  | Bool Bool
  | Proc ([Value' m] -> m (Value' m))

instance Show (Value' m) where
  show (Int n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show (Proc _) = "<procedure>"

type MonadInterp m = (MonadThrow m, MonadIO m)

eval :: MonadInterp m => AST.Exp -> m (Value' m)
eval (AST.Atom a) = evalAtom a
eval (AST.App e es) = do
  func <- eval e >>= expectProc
  args <- traverse eval es
  func args

evalAtom :: MonadInterp m => AST.Atom -> m (Value' m)
evalAtom (AST.Int n) = pure $! Int n
evalAtom (AST.Bool b) = pure $! Bool b
evalAtom (AST.Id i) =
  defaultEnv >>= liftIO . flip HT.lookup i >>= \case
    Just v -> pure v
    Nothing -> throw (EvalError "unknown id")

expectInt :: MonadInterp m => Value' m -> m Integer
expectInt (Int n) = pure n
expectInt _ = throw (EvalError "expect number")

expectBool :: MonadInterp m => Value' m -> m Bool
expectBool (Bool b) = pure b
expectBool _ = throw (EvalError "expect boolean")

expectProc :: MonadInterp m => Value' m -> m ([Value' m] -> m (Value' m))
expectProc (Proc f) = pure f
expectProc _ = throw (EvalError "expect procedure")

type Env m = BasicHashTable AST.Id (Value' m)

defaultEnv :: MonadInterp m => m (Env m)
defaultEnv = liftIO do
  env <- HT.new

  HT.insert env "number?" $
    Proc \case
      [Bool _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert env "boolean?" $
    Proc \case
      [Bool _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert env "procedure?" $
    Proc \case
      [Proc _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert env "+" $
    Proc (fmap (Int . sum) . traverse expectInt)

  HT.insert env "-" $
    Proc $
      traverse expectInt >=> \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n - sum ns)

  HT.insert env "*" $
    Proc (fmap (Int . product) . traverse expectInt)

  HT.insert env "/" $
    Proc $
      traverse expectInt >=> \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n `div` product ns)

  HT.insert env "=" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 == n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert env ">" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 > n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert env ">=" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 >= n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert env "<" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 < n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert env "<=" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 <= n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert env "not" $
    Proc \case
      [v] -> Bool . not <$!> expectBool v
      _ -> throw (EvalError "illegal number of arguments")

  pure $! env
