{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Hasche.Pattern where

import Control.Monad
import Data.Foldable
import Data.Functor.Compose
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Hasche.Eval
import Hasche.Object
import Hasche.SExpr

-- Pattern

data Pattern
  = PIgnore
  | PVar Text
  | PEmpty
  | PBool Bool
  | PNum Integer
  | PStr Text
  | PSym Text
  | PPred Text Pattern
  | PCons Pattern Pattern
  | PRest Pattern

parsePattern :: SExpr -> Pattern
parsePattern SEmpty = PEmpty
parsePattern (SBool b) = PBool b
parsePattern (SNum n) = PNum n
parsePattern (SStr s) = PStr s
parsePattern (SSym "_") = PIgnore
parsePattern (SSym s) = PVar s
parsePattern (SQuote SEmpty) = PEmpty
parsePattern (SQuote (SBool b)) = PBool b
parsePattern (SQuote (SNum n)) = PNum n
parsePattern (SQuote (SStr s)) = PStr s
parsePattern (SQuote (SSym s)) = PSym s
parsePattern (SQuote (SCons e1 e2)) =
  PCons (parsePattern $ SQuote e1) (parsePattern $ SQuote e2)
parsePattern (SSym "?" `SCons` SSym f `SCons` e `SCons` SEmpty) =
  PPred f (parsePattern e)
parsePattern (e1 `SCons` SSym "..." `SCons` SEmpty) = PRest (parsePattern e1)
parsePattern (SCons e1 e2) = PCons (parsePattern e1) (parsePattern e2)

matcher :: MonadEval m => Pattern -> Env m -> Object m -> m (Maybe (Map Text (Object m)))
matcher PIgnore = \_ _ -> pure . Just $ M.empty
matcher (PVar v) = \_ o -> pure . Just $ M.singleton v o
matcher PEmpty = \_ o -> case o of
  Empty -> pure . Just $ M.empty
  _ -> pure Nothing
matcher (PBool b) = \_ o -> case o of
  Bool b' | b == b' -> pure . Just $ M.empty
  _ -> pure Nothing
matcher (PNum n) = \_ o -> case o of
  Num n' | n == n' -> pure . Just $ M.empty
  _ -> pure Nothing
matcher (PStr s) = \_ o -> case o of
  Str s' | s == s' -> pure . Just $ M.empty
  _ -> pure Nothing
matcher (PSym s) = \_ o -> case o of
  Sym s' | s == s' -> pure . Just $ M.empty
  _ -> pure Nothing
matcher (PPred s p) =
  let m = matcher p
   in \env o -> do
        f <- eval env (SSym s)
        t <- apply f [o]
        case t of
          Bool True -> m env o
          _ -> pure Nothing
matcher (PCons p1 p2) =
  let m1 = matcher p1
      m2 = matcher p2
   in \env o -> case o of
        Cons r1 r2 -> do
          mb1 <- deref r1 >>= m1 env
          mb2 <- deref r2 >>= m2 env
          pure $ liftM2 M.union mb1 mb2
        _ -> pure Nothing
matcher (PRest p) =
  let m = matcher p
   in \env o -> do
        mos <- listify o
        case mos of
          Nothing -> pure Nothing
          Just os -> do
            mbs <- getCompose $ traverse (Compose . m env) os
            case mbs of
              Nothing -> pure Nothing
              Just bs ->
                fmap Just
                  . traverse (foldrM cons empty)
                  . M.unionsWith (++)
                  . map (M.map pure)
                  $ bs

listify :: MonadEval m => Object m -> m (Maybe [Object m])
listify Empty = pure . Just $ []
listify (Cons r1 r2) = do
  o <- deref r1
  mos <- deref r2 >>= listify
  pure $ (o :) <$> mos
listify _ = pure Nothing