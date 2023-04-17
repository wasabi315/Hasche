module Language.Hasche.Pattern (match) where

import Control.Applicative hiding (empty)
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function
import Data.Functor.Compose
import Data.List.NonEmpty qualified as NE
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Traversable
import Language.Hasche.Error
import Language.Hasche.Eval
import Language.Hasche.Object
import Prelude hiding (pred)

match ::
  (MonadIO m, MonadThrow m) =>
  Object (EvalT r m) ->
  [(Object (EvalT r m), Object (EvalT r m))] ->
  EvalT r m (Object (EvalT r m))
match expr arms = do
  obj <- eval expr
  arms' <- for arms \(pat, body) -> do
    parsedPat <- parsePattern pat
    binds <- matcher parsedPat obj
    pure $ fmap (,body) binds
  case asum arms' of
    Nothing -> pure undef
    Just (binds, body) -> withNewScope do
      void $ M.traverseWithKey bind binds
      eval body

-- Pattern

data Pattern
  = PIgnore
  | PVar T.Text
  | PEmpty
  | PBool Bool
  | PNum Integer
  | PStr T.Text
  | PSym T.Text
  | PPred T.Text Pattern
  | PCons Pattern Pattern
  | PRest Pattern
  deriving (Show)

parsePattern ::
  (MonadIO m, MonadThrow m) => Object (EvalT r m) -> EvalT r m Pattern
parsePattern =
  listify >=> \case
    NonList (Bool b) -> pure $ PBool b
    NonList (Num n) -> pure $ PNum n
    NonList (Str s) -> pure $ PStr s
    NonList (Sym "_") -> pure PIgnore
    NonList (Sym s) -> pure $ PVar s
    NonList _ -> throw ESyntax
    List [] -> pure PEmpty
    List [Sym "quote", obj] -> parseSExprPattern obj
    List [Sym "quasiquote", obj] -> parseQuasiPattern obj
    List [Sym "?", Sym pred, obj] -> PPred pred <$> parsePattern obj
    List [obj, Sym "..."] -> PRest <$> parsePattern obj
    List (obj : objs) -> do
      pat <- parsePattern obj
      pat' <- parsePattern =<< list objs
      pure $ PCons pat pat'
    DList (obj NE.:| objs) obj' -> do
      pat <- parsePattern obj
      pats <- parsePattern =<< case objs of
        [] -> pure obj'
        o : os -> dlist (o NE.:| os) obj'
      pure $ PCons pat pats

parseSExprPattern ::
  (MonadIO m, MonadThrow m) => Object (EvalT r m) -> EvalT r m Pattern
parseSExprPattern Empty = pure PEmpty
parseSExprPattern (Bool b) = pure $ PBool b
parseSExprPattern (Num n) = pure $ PNum n
parseSExprPattern (Str s) = pure $ PStr s
parseSExprPattern (Sym s) = pure $ PSym s
parseSExprPattern (Cons ref1 re2) = do
  pat1 <- parseSExprPattern =<< deref ref1
  pat2 <- parseSExprPattern =<< deref re2
  pure $ PCons pat1 pat2
parseSExprPattern _ = throw ESyntax

parseQuasiPattern ::
  (MonadIO m, MonadThrow m) => Object (EvalT r m) -> EvalT r m Pattern
parseQuasiPattern =
  listify >=> \case
    NonList (Bool b) -> pure $ PBool b
    NonList (Num n) -> pure $ PNum n
    NonList (Str s) -> pure $ PStr s
    NonList (Sym s) -> pure $ PSym s
    NonList _ -> throw ESyntax
    List [] -> pure PEmpty
    List [Sym "unquote", obj] -> parsePattern obj
    List (obj : objs) -> do
      pat <- parsePattern obj
      pat' <- parsePattern =<< list objs
      pure $ PCons pat pat'
    DList (obj NE.:| objs) obj' -> do
      pat <- parseQuasiPattern obj
      pats <- parseQuasiPattern =<< case objs of
        [] -> pure obj'
        o : os -> dlist (o NE.:| os) obj'
      pure $ PCons pat pats

matcher ::
  (MonadIO m, MonadThrow m) =>
  Pattern ->
  Object (EvalT r m) ->
  EvalT r m (Maybe (M.Map T.Text (Object (EvalT r m))))
matcher (PVar var) obj = pure . Just $ M.singleton var obj
matcher PIgnore _ = pure . Just $ M.empty
matcher PEmpty Empty = pure . Just $ M.empty
matcher (PBool b) (Bool b')
  | b == b' = pure . Just $ M.empty
matcher (PNum n) (Num n')
  | n == n' = pure . Just $ M.empty
matcher (PStr s) (Str s')
  | s == s' = pure . Just $ M.empty
matcher (PSym s) (Sym s')
  | s == s' = pure . Just $ M.empty
matcher (PPred pred pat) obj = do
  pred' <- eval =<< sym pred
  t <- apply pred' [obj]
  case t of
    Bool False -> pure Nothing
    _ -> matcher pat obj
matcher (PCons pat1 pat2) (Cons ref1 ref2) = do
  binder1 <- deref ref1 >>= matcher pat1
  binder2 <- deref ref2 >>= matcher pat2
  pure $ liftA2 M.union binder1 binder2
matcher (PRest pat) obj = do
  listify obj >>= \case
    List objs@(_ : _) -> do
      maybeBinds <- getCompose $ traverse (Compose . matcher pat) objs
      traverse mergeBinds maybeBinds
    _ -> pure Nothing
matcher _ _ = pure Nothing

mergeBinds ::
  MonadIO m =>
  [M.Map T.Text (Object (EvalT r m))] ->
  EvalT r m (M.Map T.Text (Object (EvalT r m)))
mergeBinds = foldrM mergeBinds' M.empty
  where
    mergeBinds' =
      M.mergeA
        (M.traverseMissing $ const (`cons` empty))
        M.dropMissing
        (M.zipWithAMatched $ const cons)
