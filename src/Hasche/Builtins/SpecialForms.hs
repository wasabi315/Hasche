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
import Data.Maybe
import Data.Text (Text)
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

    uqExpr (SList [SSym "unquote", e] Nothing) = Just e
    uqExpr _ = Nothing
    uqsExpr (SList [SSym "unquote-splicing", e] Nothing) = Just e
    uqsExpr _ = Nothing

    expectList :: MonadEval m => Object m -> m [Object m]
    expectList = \case
      Empty -> pure []
      Cons car cdr -> do
        o1 <- deref car
        o2 <- deref cdr
        (o1 :) <$> expectList o2
      _ -> throw (EvalError "expect list")

    qq :: MonadEval m => Env m -> SExpr -> m (Object m)
    qq _ (SBool b) = pure if b then true else false
    qq _ (SNum n) = num n
    qq _ (SStr s) = str s
    qq _ (SSym s) = sym s
    qq env (uqExpr -> Just e) = eval env e
    qq _ (uqsExpr -> Just _) = throw (SynError "invalid unquote-splicing context")
    qq env (SList es me) = do
      os <- concat <$> traverse (qqs env) es
      mo <- traverse (qq env) me
      foldrM cons (fromMaybe empty mo) os

    qqs env (uqsExpr -> Just e) = eval env e >>= expectList
    qqs env e = pure <$> qq env e

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
    define env (SList (SSym s : ps) mp : b) = do
      obj <- mkClosure env (SList ps mp) b
      undef <$ bind env s obj
    define _ _ = throw (SynError "Illegal define syntax")

synDefMacro :: (MonadIO m, MonadEval n) => m (Object n)
synDefMacro = syn defMacro
  where
    defMacro env es = do
      (s, o1) <- case es of
        [SSym s, e] -> (s,) <$> eval env e
        (SList (SSym s : ps) mp : b) -> (s,) <$> mkClosure env (SList ps mp) b
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
  case extractParams e of
    Nothing -> throw (SynError "Illegal parameters")
    Just (ps, rest) -> do
      unless (isValidBody b) do
        throw (SynError "define cannot appear after expressions")
      func \args -> do
        env' <- childEnv env
        bindArgs env' ps rest args
        evalMany env' b
  where
    extractParams :: SExpr -> Maybe ([Text], Maybe Text)
    extractParams (SSym s) = Just ([], Just s)
    extractParams (SList es me) = do
      ss <- traverse expectSSym es
      ms <- traverse expectSSym me
      pure (ss, ms)
    extractParams _ = Nothing

    expectSSym :: SExpr -> Maybe Text
    expectSSym (SSym s) = Just s
    expectSSym _ = Nothing

    bindArgs :: MonadEval m => Env m -> [Text] -> Maybe Text -> [Object m] -> m ()
    bindArgs _ [] Nothing [] = pure ()
    bindArgs env [] (Just p) os = do
      o <- foldrM cons empty os
      bind env p o
    bindArgs env (p : ps) mp (o : os) = do
      bind env p o
      bindArgs env ps mp os
    bindArgs _ _ _ _ = throw (EvalError "Arity Mismatch")

    -- body : (define ...)* expression*
    isValidBody :: [SExpr] -> Bool
    isValidBody = isJust . foldl' phi (Just True)
      where
        phi Nothing _ = Nothing
        phi (Just isDefPart) (SList (SSym "define" : _) _) =
          if isDefPart then Just True else Nothing
        phi _ _ = Just False

-- utils

expectFunc :: (MonadIO m, MonadThrow m) => Object n -> m ([Object n] -> n (Object n))
expectFunc = \case
  Func f -> pure f
  _ -> throw (EvalError "expect procedure")
