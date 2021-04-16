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
import Data.Text.Read qualified as Text
import MiniScheme.AST qualified as AST
import Prelude hiding (lookup)

interpret :: AST.Prog -> IO (Either EvalError Value)
interpret prog =
  catch
    do
      env <- defaultEnv
      v <- runInterpreter (eval env prog)
      pure . Right $! Value v
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
  | Str Text
  | Proc (Env m) (Env m -> [Value' m] -> m (Value' m))

instance Show (Value' m) where
  show (Int n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show (Str s) = show s
  show (Proc _ _) = "<procedure>"

type MonadInterp m = (MonadThrow m, MonadIO m)

eval :: MonadInterp m => Env m -> AST.Prog -> m (Value' m)
eval env (AST.Exp e) = evalExp env e
eval env (AST.Def (AST.Const i e)) = do
  v <- evalExp env e
  bind env i v
  pure v

evalExp :: MonadInterp m => Env m -> AST.Exp -> m (Value' m)
evalExp env (AST.Atom a) = evalAtom env a
evalExp env (AST.App e es) = do
  (env', func) <- evalExp env e >>= expectProc
  args <- traverse (evalExp env) es
  func env' args

evalAtom :: MonadInterp m => Env m -> AST.Atom -> m (Value' m)
evalAtom _ (AST.Int n) = pure $! Int n
evalAtom _ (AST.Bool b) = pure $! Bool b
evalAtom _ (AST.Str s) = pure $! Str s
evalAtom env (AST.Id i) = lookup env i

expectInt :: MonadInterp m => Value' m -> m Integer
expectInt (Int n) = pure n
expectInt _ = throw (EvalError "expect number")

expectBool :: MonadInterp m => Value' m -> m Bool
expectBool (Bool b) = pure b
expectBool _ = throw (EvalError "expect boolean")

expectStr :: MonadInterp m => Value' m -> m Text
expectStr (Str s) = pure s
expectStr _ = throw (EvalError "expect string")

expectProc ::
  MonadInterp m =>
  Value' m ->
  m (Env m, Env m -> [Value' m] -> m (Value' m))
expectProc (Proc e f) = pure (e, f)
expectProc _ = throw (EvalError "expect procedure")

data Env m = Env
  { binds :: BasicHashTable AST.Id (Value' m),
    outer :: Maybe (Env m)
  }

lookup :: MonadInterp m => Env m -> AST.Id -> m (Value' m)
lookup env i = lookup' env
  where
    lookup' Env {..} = do
      liftIO (HT.lookup binds i) >>= \case
        Just v -> pure v
        Nothing -> case outer of
          Just env' -> lookup' env'
          Nothing -> throw (EvalError "Unbound identifier")

bind :: MonadInterp m => Env m -> AST.Id -> Value' m -> m ()
bind Env {..} i v = do
  declared <- liftIO $ HT.mutate binds i \case
    Just v' -> (Just v', True)
    Nothing -> (Just v, False)
  when declared do
    throw (EvalError "identifier already declared")

set :: MonadInterp m => Env m -> AST.Id -> Value' m -> m ()
set env i v = set' env
  where
    set' Env {..} = do
      done <- liftIO $ HT.mutate binds i \case
        Just _ -> (Just v, True)
        Nothing -> (Nothing, False)
      unless done do
        case outer of
          Just env' -> set' env'
          Nothing -> throw (EvalError "Unbound identifier")

defaultEnv :: MonadInterp m => m (Env m)
defaultEnv = liftIO do
  binds <- HT.new
  phi <- flip Env Nothing <$> HT.new

  HT.insert binds "number?" $
    Proc phi \_ args -> case args of
      [Int _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "boolean?" $
    Proc phi \_ args -> case args of
      [Bool _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "string?" $
    Proc phi \_ args -> case args of
      [Str _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "procedure?" $
    Proc phi \_ args -> case args of
      [Proc _ _] -> pure $! Bool True
      [_] -> pure $! Bool False
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "+" $
    Proc phi \_ args -> Int . sum <$!> traverse expectInt args

  HT.insert binds "-" $
    Proc phi \_ args ->
      traverse expectInt args >>= \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n - sum ns)

  HT.insert binds "*" $
    Proc phi \_ args -> Int . product <$!> traverse expectInt args

  HT.insert binds "/" $
    Proc phi \_ args ->
      traverse expectInt args >>= \case
        [] -> throw (EvalError "expect at least one number")
        n : ns -> pure $! Int (n `div` product ns)

  HT.insert binds "=" $
    Proc phi \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 == n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds ">" $
    Proc phi \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 > n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds ">=" $
    Proc phi \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 >= n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "<" $
    Proc phi \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 < n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "<=" $
    Proc phi \_ args ->
      traverse expectInt args >>= \case
        [n1, n2] -> pure $! Bool (n1 <= n2)
        _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "not" $
    Proc phi \_ args -> case args of
      [v] -> Bool . not <$!> expectBool v
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "string-append" $
    Proc phi $ \_ args -> Str . Text.concat <$!> traverse expectStr args

  HT.insert binds "string->number" $
    Proc phi \_ args -> case args of
      [v] ->
        expectStr v >>= \s -> case Text.signed Text.decimal s of
          Right (n, "") -> pure $! Int n
          _ -> throw (EvalError "Failed to convert string->number")
      _ -> throw (EvalError "illegal number of arguments")

  HT.insert binds "number->string" $
    Proc phi \_ args -> case args of
      [v] -> expectInt v >>= \n -> pure $! Str (Text.pack (show n))
      _ -> throw (EvalError "illegal number of arguments")

  pure $! Env binds Nothing
