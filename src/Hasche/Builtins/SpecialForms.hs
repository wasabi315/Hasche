{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Hasche.Builtins.SpecialForms where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Cont
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Hasche.Cell
import Hasche.Eval
import Hasche.Object
import Hasche.SExpr
import Prelude hiding (lookup)

-- special forms

synQuote :: (MonadIO m, MonadEval n) => m (Object n)
synQuote = syn quote
  where
    quote _ [e] = fromSExpr e
    quote _ _ = throw (SynError "Illegal quote syntax")

synQuasiquote :: (MonadIO m, MonadEval n) => m (Object n)
synQuasiquote = syn quasiquote
  where
    quasiquote env [e] = qq env e
    quasiquote _ _ = throw (SynError "Illegal quasiquote syntax")

    expectList :: MonadEval m => Object m -> m [Object m]
    expectList = \case
      Empty -> pure []
      Cons car cdr -> do
        o1 <- deref car
        o2 <- deref cdr
        (o1 :) <$> expectList o2
      _ -> throw (EvalError "expect list")

    qq :: MonadEval m => Env m -> SExpr -> m (Object m)
    qq _ SEmpty = pure empty
    qq _ (SBool b) = pure if b then true else false
    qq _ (SNum n) = num n
    qq _ (SStr s) = str s
    qq _ (SSym s) = sym s
    qq env (SUQ e) = eval env e
    qq _ (SUQS _) = throw (SynError "invalid unquote-splicing context")
    qq env (SUQS e1 `SCons` e2) = do
      os1 <- eval env e1 >>= expectList
      os2 <- qq env e2
      foldrM cons os2 os1
    qq env (e1 `SCons` e2) = do
      os1 <- qq env e1
      os2 <- qq env e2
      cons os1 os2

synUnquote :: (MonadIO m, MonadEval n) => m (Object n)
synUnquote = syn unquote
  where
    unquote _ _ = throw (SynError "unquote appeared outside quasiquote")

synUnquoteSplicing :: (MonadIO m, MonadEval n) => m (Object n)
synUnquoteSplicing = syn unquoteSplicing
  where
    unquoteSplicing _ _ = throw (SynError "unquote-splicing appeared outside quasiquote")

synIf :: (MonadIO m, MonadEval n) => m (Object n)
synIf = syn if_
  where
    if_ env [e0, e1] = do
      b <- eval env e0
      case b of
        Bool False -> pure undef
        _ -> eval env e1
    if_ env [e0, e1, e2] = do
      b <- eval env e0
      case b of
        Bool False -> eval env e2
        _ -> eval env e1
    if_ _ _ = throw (SynError "Illegal if syntax")

synSet :: (MonadIO m, MonadEval n) => m (Object n)
synSet = syn set
  where
    set env [SSym s, e] = do
      obj <- eval env e
      lookup env s >>= \case
        Nothing -> throw (EvalError $ "Unbound identifier: " <> s)
        Just ref -> undef <$ (ref .= obj)
    set _ _ = throw (SynError "Illegal set! syntax")

synDefine :: (MonadIO m, MonadEval n) => m (Object n)
synDefine = syn define
  where
    define env [SSym s, e] = do
      obj <- eval env e
      undef <$ bind env s obj
    define env (SCons (SSym s) ps : b) = do
      obj <- mkClosure env ps b
      undef <$ bind env s obj
    define _ _ = throw (SynError "Illegal define syntax")

synDefMacro :: (MonadIO m, MonadEval n) => m (Object n)
synDefMacro = syn defMacro
  where
    defMacro env es = do
      (s, o1) <- case es of
        [SSym s, e] -> (s,) <$> eval env e
        (SCons (SSym s) ps : b) -> (s,) <$> mkClosure env ps b
        _ -> throw (SynError "Illegal define-macro syntax")
      t <- expectFunc o1
      o2 <- syn \env'' es' -> do
        me <- traverse fromSExpr es' >>= t >>= toSExpr
        case me of
          Nothing -> throw (EvalError "Failed to expand macro")
          Just e -> eval env'' e
      undef <$ bind env s o2

synLambda :: (MonadIO m, MonadEval n) => m (Object n)
synLambda = syn lambda
  where
    lambda env (ps : b) = mkClosure env ps b
    lambda _ _ = throw (SynError "Illegal lambda syntax")

mkClosure :: MonadEval m => Env m -> SExpr -> [SExpr] -> m (Object m)
mkClosure = \env e b -> do
  binder <- argBinder e
  unless (isValidBody b) do
    throw (SynError "define cannot appear after expressions")
  func \args -> do
    env' <- childEnv env
    binder env' args
    evalMany env' b
  where
    argBinder :: MonadEval m => SExpr -> m (Env m -> [Object m] -> m ())
    argBinder SEmpty = pure \_ os -> case os of
      [] -> pure ()
      _ -> throw (EvalError "Arity Mismatcch")
    argBinder (SSym s) = pure \env os ->
      foldrM cons empty os >>= bind env s
    argBinder (SSym s `SCons` e) = do
      binder <- argBinder e
      pure \env os -> case os of
        [] -> throw (EvalError "Arity Mismatch")
        o : os' -> bind env s o *> binder env os'
    argBinder _ = throw (SynError "Illegal parameters")

    -- body : (define ...)* expression*
    isValidBody :: [SExpr] -> Bool
    isValidBody = isJust . foldl' phi (Just True)
      where
        phi Nothing _ = Nothing
        phi (Just isDefPart) (SList (SSym "define" NE.:| _)) =
          if isDefPart then Just True else Nothing
        phi _ _ = Just False

-- utils

expectFunc :: (MonadIO m, MonadThrow m) => Object n -> m ([Object n] -> n (Object n))
expectFunc = \case
  Func f -> pure f
  _ -> throw (EvalError "expect procedure")
