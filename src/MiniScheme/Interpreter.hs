{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Prelude hiding (lookup)

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
evalAtom (AST.Id i) = defaultEnv >>= flip lookup i

expectInt :: MonadInterp m => Value' m -> m Integer
expectInt (Int n) = pure n
expectInt _ = throw (EvalError "expect number")

expectBool :: MonadInterp m => Value' m -> m Bool
expectBool (Bool b) = pure b
expectBool _ = throw (EvalError "expect boolean")

expectProc :: MonadInterp m => Value' m -> m ([Value' m] -> m (Value' m))
expectProc (Proc f) = pure f
expectProc _ = throw (EvalError "expect procedure")

data Env m = Env
  { binds :: BasicHashTable AST.Id (Value' m),
    outer :: Maybe (Env m)
  }

lookup :: MonadInterp m => Env m -> AST.Id -> m (Value' m)
lookup Env {..} i =
  liftIO (HT.lookup binds i) >>= \case
    Just v -> pure v
    Nothing -> case outer of
      Nothing -> throw (EvalError "Unbound identifier")
      Just env -> lookup env i

set :: MonadInterp m => Env m -> AST.Id -> Value' m -> m ()
set Env {..} i v = liftIO $ HT.insert binds i v

defaultEnv :: MonadInterp m => m (Env m)
defaultEnv = liftIO do
  binds <- HT.new

  HT.insert binds "number?" $
    Proc \case
      [Bool _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "boolean?" $
    Proc \case
      [Bool _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "procedure?" $
    Proc \case
      [Proc _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "+" $
    Proc (fmap (Int . sum) . traverse expectInt)

  HT.insert binds "-" $
    Proc $
      traverse expectInt >=> \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n - sum ns)

  HT.insert binds "*" $
    Proc (fmap (Int . product) . traverse expectInt)

  HT.insert binds "/" $
    Proc $
      traverse expectInt >=> \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n `div` product ns)

  HT.insert binds "=" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 == n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds ">" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 > n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds ">=" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 >= n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "<" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 < n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "<=" $
    Proc $
      traverse expectInt >=> \case
        [n1, n2] -> pure $! Bool (n1 <= n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "not" $
    Proc \case
      [v] -> Bool . not <$!> expectBool v
      _ -> throw (EvalError "illegal number of arguments")

  pure $! Env binds Nothing
