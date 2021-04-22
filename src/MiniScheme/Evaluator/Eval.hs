{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Evaluator.Eval
  ( eval,
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.List.NonEmpty qualified as NE
import MiniScheme.AST qualified as AST
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Monad
import Prelude hiding (lookup)

eval :: MonadEval m => Env m -> [AST.Prog] -> m (Value' m)
eval env = fmap (maybe empty NE.last . NE.nonEmpty) . traverse (evalProg env)

evalProg :: MonadEval m => Env m -> AST.Prog -> m (Value' m)
evalProg env (AST.Exp e) = evalExp env e
evalProg env (AST.Def def) = evalDef env def

evalDef :: MonadEval m => Env m -> AST.Def -> m (Value' m)
evalDef env (AST.Const i e) = do
  v <- evalExp env e
  bind env i v
  pure empty

evalExp :: MonadEval m => Env m -> AST.Exp -> m (Value' m)
evalExp env (AST.Atom a) = evalAtom env a
evalExp env (AST.Set i e) = do
  v <- evalExp env e
  set env i v
  pure v
evalExp env (AST.If p t e) = do
  evalExp env p >>= \v -> case val v of
    Bool False -> maybe (pure undef) (evalExp env) e
    _ -> evalExp env t
evalExp env (AST.Lam args body) =
  alloc $ Proc env \env' vs -> do
    env'' <- childEnv env'
    when (length args /= length vs) do
      throw (EvalError "illegal number of arguments")
    zipWithM_ (bind env'') args vs
    evalBody env'' body
evalExp env (AST.Let mname binds body) = do
  env' <- childEnv env
  traverse_ (\(x, e) -> evalExp env e >>= bind env' x) binds
  for_ mname \name ->
    bind env' name
      =<< alloc
        ( Proc env' \env'' vs -> do
            when (length binds /= length vs) do
              throw (EvalError "illegal number of arguments")
            zipWithM_ (set env'') (map fst binds) vs
            evalBody env'' body
        )
  evalBody env' body
evalExp env (AST.LetA mname binds body) = do
  env' <-
    foldM
      ( \env' (x, e) -> do
          env'' <- childEnv env'
          evalExp env' e >>= bind env'' x
          pure env''
      )
      env
      binds
  for_ mname \name ->
    bind env' name
      =<< alloc
        ( Proc env' \env'' vs -> do
            when (length binds /= length vs) do
              throw (EvalError "illegal number of arguments")
            zipWithM_ (set env'') (map fst binds) vs
            evalBody env'' body
        )
  evalBody env' body
evalExp env (AST.LetRec mname binds body) = do
  env' <- childEnv env
  traverse_ (\(x, _) -> bind env' x undef) binds
  vs <- traverse (\(_, e) -> evalExp env' e) binds
  zipWithM_ (set env') (map fst binds) vs
  for_ mname \name ->
    bind env' name
      =<< alloc
        ( Proc env' \env'' vs' -> do
            when (length binds /= length vs) do
              throw (EvalError "illegal number of arguments")
            zipWithM_ (set env'') (map fst binds) vs'
            evalBody env'' body
        )
  evalBody env' body
evalExp env (AST.Begin es) = do
  vs <- traverse (evalExp env) es
  pure $! maybe empty NE.last (NE.nonEmpty vs)
evalExp env (AST.App e es) = do
  (env', func) <- evalExp env e >>= expectProc
  args <- traverse (evalExp env) es
  func env' args

evalBody :: MonadEval m => Env m -> AST.Body -> m (Value' m)
evalBody env (AST.Body ds es) = do
  traverse_ (evalDef env) ds
  vs <- traverse (evalExp env) es
  pure $! NE.last vs

evalAtom :: MonadEval m => Env m -> AST.Atom -> m (Value' m)
evalAtom _ AST.Empty = pure empty
evalAtom _ (AST.Num n) = alloc (Num n)
evalAtom _ (AST.Bool b) = alloc (Bool b)
evalAtom _ (AST.Str s) = alloc (Str s)
evalAtom env (AST.Id i) = lookup env i >>= liftIO . readIORef
