{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasche.Builtins.SpecialForms where

import Control.Exception.Safe
import Control.Monad.Cont
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Hasche.Eval
import Hasche.Object
import Hasche.SExpr
import Prelude hiding (lookup)

-- special forms

synQuote :: (MonadIO m, MonadEval n) => m (ObjRef n)
synQuote = syn quote
  where
    quote _ [e] = toData e
    quote _ _ = throw (SynError "Illegal quote syntax")

synIf :: (MonadIO m, MonadEval n) => m (ObjRef n)
synIf = syn if_
  where
    if_ env [e0, e1] = do
      b <- eval env e0 >>= deref
      case b of
        Bool False -> pure undef
        _ -> eval env e1
    if_ env [e0, e1, e2] = do
      b <- eval env e0 >>= deref
      case b of
        Bool False -> eval env e2
        _ -> eval env e1
    if_ _ _ = throw (SynError "Illegal if syntax")

synSet :: (MonadIO m, MonadEval n) => m (ObjRef n)
synSet = syn set
  where
    set env [SSym s, e] = do
      obj <- eval env e >>= deref
      lookup env s >>= \case
        Nothing -> throw (EvalError $! "Unbound identifier: " <> s)
        Just ref -> undef <$ (ref .= obj)
    set _ _ = throw (SynError "Illegal set! syntax")

synDefine :: (MonadIO m, MonadEval n) => m (ObjRef n)
synDefine = syn define
  where
    define env [SSym s, e] = do
      obj <- eval env e
      undef <$ bind env s obj
    define env (SList (SSym s : ps) mp : b) = do
      obj <- mkClosure env (SList ps mp) b
      undef <$ bind env s obj
    define _ _ = throw (SynError "Illegal define syntax")

synLambda :: (MonadIO m, MonadEval n) => m (ObjRef n)
synLambda = syn lambda
  where
    lambda env (ps : b) = mkClosure env ps b
    lambda _ _ = throw (SynError "Illegal lambda syntax")

mkClosure :: MonadEval m => Env m -> SExpr -> [SExpr] -> m (ObjRef m)
mkClosure = \env e b -> do
  case extractParams e of
    Nothing -> throw (SynError "Illegal parameters")
    Just (ps, rest) -> do
      unless (isValidBody b) do
        throw (SynError "define cannot appear after expressions")
      func env \env' args -> do
        env'' <- childEnv env'
        bindArgs env'' ps rest args
        evalMany env'' b
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

    bindArgs :: MonadEval m => Env m -> [Text] -> Maybe Text -> [ObjRef m] -> m ()
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

-- SExpr -> Object
toData :: MonadIO m => SExpr -> m (ObjRef n)
toData (SBool b) = pure if b then true else false
toData (SNum n) = num n
toData (SStr s) = str s
toData (SSym s) = sym s
toData (SList es me) = do
  os <- traverse toData es
  mo <- traverse toData me
  foldrM cons (fromMaybe empty mo) os
