{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Interpreter.Interpreter
  ( eval,
  )
where

import Control.Exception.Safe
import Control.Monad
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import MiniScheme.AST qualified as AST
import MiniScheme.Interpreter.Data
import MiniScheme.Interpreter.Monad
import Prelude hiding (lookup)

eval :: MonadInterp m => Env' m -> AST.Prog -> m (Value' m)
eval env (AST.Exp e) = evalExp env e
eval env (AST.Def def) = evalDef env def

evalDef :: MonadInterp m => Env' m -> AST.Def -> m (Value' m)
evalDef env (AST.Const i e) = do
  v <- evalExp env e
  bind env i v
  pure v

evalExp :: MonadInterp m => Env' m -> AST.Exp -> m (Value' m)
evalExp env (AST.Atom a) = evalAtom env a
evalExp env (AST.Set i e) = do
  v <- evalExp env e
  set env i v
  pure v
evalExp env (AST.Lam args body) =
  pure $! Proc env \env' vs -> do
    env'' <- childEnv env'
    when (length args /= length vs) do
      throw (EvalError "illegal number of arguments")
    zipWithM_ (bind env'') args vs
    evalBody env'' body
evalExp env (AST.App e es) = do
  (env', func) <- evalExp env e >>= expectProc
  args <- traverse (evalExp env) es
  func env' args

evalBody :: MonadInterp m => Env' m -> AST.Body -> m (Value' m)
evalBody env (AST.Body ds es) = do
  traverse_ (evalDef env) ds
  vs <- traverse (evalExp env) es
  pure $! NE.last vs

evalAtom :: MonadInterp m => Env' m -> AST.Atom -> m (Value' m)
evalAtom _ (AST.Int n) = pure $! Int n
evalAtom _ (AST.Bool b) = pure $! Bool b
evalAtom _ (AST.Str s) = pure $! Str s
evalAtom env (AST.Id i) = lookup env i
