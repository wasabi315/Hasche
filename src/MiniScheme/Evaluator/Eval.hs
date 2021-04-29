{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Evaluator.Eval
  ( eval,
    apply,
    cons,
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.AST qualified as AST
import MiniScheme.Evaluator.Data
import MiniScheme.Evaluator.Monad
import MiniScheme.Parser
import Prelude hiding (lookup)

eval :: MonadEval m => Env m -> [AST.Prog] -> m (Value m)
eval env = fmap (maybe empty NE.last . NE.nonEmpty) . traverse (evalProg env)

evalProg :: MonadEval m => Env m -> AST.Prog -> m (Value m)
evalProg env (AST.Exp e) = evalExp env e
evalProg env (AST.Def def) = evalDef env def
evalProg env (AST.Load path) = do
  let path' = Text.unpack path
  txt <- liftIO (Text.readFile path')
  case parseProg path' txt of
    Left err -> throw (EvalError (Text.pack (show err)))
    Right prog -> eval env prog

evalDef :: MonadEval m => Env m -> AST.Def -> m (Value m)
evalDef env (AST.Const i e) = do
  v <- evalExp env e
  bind env i v
  pure empty

evalExp :: MonadEval m => Env m -> AST.Exp -> m (Value m)
evalExp env (AST.Atom a) = evalAtom env a
evalExp _ (AST.Quote e) = evalSExp e
evalExp env (AST.Set i e) = do
  v <- evalExp env e
  set env i v
  pure v
evalExp env (AST.If p t e) = do
  evalExp env p >>= \v -> case val v of
    Bool False -> maybe (pure undef) (evalExp env) e
    _ -> evalExp env t
evalExp env (AST.App e es) = do
  v <- evalExp env e
  vs <- traverse (evalExp env) es
  apply env v vs
evalExp env (AST.Lam (AST.Args args rest) body) = do
  let bindArgs _ [] [] Nothing = pure ()
      bindArgs env' vs [] (Just a) = foldrM cons empty vs >>= bind env' a
      bindArgs env' (v : vs) (a : as) r = bind env' a v *> bindArgs env' vs as r
      bindArgs _ _ _ _ = throw (EvalError "illegal number of arguments")
  alloc $ Proc env \env' vs -> do
    env'' <- childEnv env'
    bindArgs env'' vs args rest
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

evalBody :: MonadEval m => Env m -> AST.Body -> m (Value m)
evalBody env (AST.Body ds es) = do
  traverse_ (evalDef env) ds
  vs <- traverse (evalExp env) es
  pure $! NE.last vs

evalAtom :: MonadEval m => Env m -> AST.Atom -> m (Value m)
evalAtom _ AST.Empty = pure empty
evalAtom _ (AST.Num n) = alloc (Num n)
evalAtom _ (AST.Bool b) = alloc (Bool b)
evalAtom _ (AST.Str s) = alloc (Str s)
evalAtom env (AST.Id i) = lookup env i >>= liftIO . readIORef

evalSExp :: MonadEval m => AST.SExp -> m (Value m)
evalSExp (AST.SAtom AST.Empty) = pure empty
evalSExp (AST.SAtom (AST.Num n)) = alloc (Num n)
evalSExp (AST.SAtom (AST.Bool b)) = alloc (Bool b)
evalSExp (AST.SAtom (AST.Str s)) = alloc (Str s)
evalSExp (AST.SAtom (AST.Id i)) = strToSym i
evalSExp (AST.SPair e1 e2) = do
  v1 <- evalSExp e1
  v2 <- evalSExp e2
  cons v1 v2

cons :: MonadEval m => Value m -> Value m -> m (Value m)
cons v1 v2 = do
  r1 <- liftIO (newIORef v1)
  r2 <- liftIO (newIORef v2)
  alloc $ Pair r1 r2

apply :: MonadEval m => Env m -> Value m -> [Value m] -> m (Value m)
apply env f xs =
  case val f of
    Prim prim -> prim env xs
    Proc env' func -> func env' xs
    Cont cont ->
      case xs of
        [x] -> cont x
        _ -> throw (EvalError "illegal number of arguments")
    _ -> do
      throw (EvalError "expect procedure or continuation")
