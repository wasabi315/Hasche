{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Language.Hasche.Eval.Pattern
  ( matcher,
    parsePattern,
  )
where

import Control.Monad
import Data.Cell qualified as Cell
import Data.Foldable
import Data.Functor.Compose
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Language.Hasche.Eval.Eval
import Language.Hasche.Eval.Object
import Language.Hasche.Syntax.SExpr

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
parsePattern (SQuote e) = parseSExprPattern e
parsePattern (SQQ e) = parseQuasiPattern e
parsePattern (SSym "?" `SCons` SSym f `SCons` e `SCons` SEmpty) =
  PPred f (parsePattern e)
parsePattern (e1 `SCons` SSym "..." `SCons` SEmpty) = PRest (parsePattern e1)
parsePattern (SCons e1 e2) = PCons (parsePattern e1) (parsePattern e2)

parseSExprPattern :: SExpr -> Pattern
parseSExprPattern SEmpty = PEmpty
parseSExprPattern (SBool b) = PBool b
parseSExprPattern (SNum n) = PNum n
parseSExprPattern (SStr s) = PStr s
parseSExprPattern (SSym s) = PSym s
parseSExprPattern (SCons e1 e2) =
  PCons (parseSExprPattern e1) (parseSExprPattern e2)

parseQuasiPattern :: SExpr -> Pattern
parseQuasiPattern SEmpty = PEmpty
parseQuasiPattern (SBool b) = PBool b
parseQuasiPattern (SNum n) = PNum n
parseQuasiPattern (SStr s) = PStr s
parseQuasiPattern (SSym s) = PSym s
parseQuasiPattern (SUQ e) = parsePattern e
parseQuasiPattern (SCons e1 e2) =
  PCons (parseQuasiPattern e1) (parseQuasiPattern e2)

matcher :: MonadEval m => Pattern -> Env m -> Object m -> m (Maybe (Map Text (Object m)))
matcher PIgnore _ _ = pure . Just $ M.empty
matcher (PVar v) _ o = pure . Just $ M.singleton v o
matcher PEmpty _ Empty = pure . Just $ M.empty
matcher (PBool b) _ (Bool b')
  | b == b' = pure . Just $ M.empty
matcher (PNum n) _ (Num n')
  | n == n' = pure . Just $ M.empty
matcher (PStr s) _ (Str s')
  | s == s' = pure . Just $ M.empty
matcher (PSym s) _ (Sym s')
  | s == s' = pure . Just $ M.empty
matcher (PPred s p) env o = do
  f <- eval env (SSym s)
  t <- apply f [o]
  case t of
    Bool False -> pure Nothing
    _ -> matcher p env o
matcher (PCons p1 p2) env (Cons r1 r2) = do
  mb1 <- Cell.deref r1 >>= matcher p1 env
  mb2 <- Cell.deref r2 >>= matcher p2 env
  pure $ liftM2 M.union mb1 mb2
matcher (PRest p) env o = do
  mos <- listify o
  case mos of
    Nothing -> pure Nothing
    Just [] -> pure Nothing
    Just os -> do
      mbs <- getCompose $ traverse (Compose . matcher p env) os
      traverse mergeBinds mbs
matcher _ _ _ = pure Nothing

listify :: MonadEval m => Object m -> m (Maybe [Object m])
listify Empty = pure . Just $ []
listify (Cons r1 r2) = do
  o <- Cell.deref r1
  mos <- Cell.deref r2 >>= listify
  pure $ (o :) <$> mos
listify _ = pure Nothing

mergeBinds ::
  MonadEval m =>
  [Map Text (Object m)] ->
  m (Map Text (Object m))
mergeBinds = foldrM mergeBinds' M.empty
  where
    mergeBinds' =
      M.mergeA
        (M.traverseMissing $ const (`cons` empty))
        M.dropMissing -- will not be used
        (M.zipWithAMatched $ const cons)
