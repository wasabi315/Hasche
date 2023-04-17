module Language.Hasche.Pattern (match) where

import Control.Applicative (Applicative (liftA2))
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable
import Data.Function
import Data.Functor.Compose
import Data.Text qualified as T
import Data.Traversable
import Language.Hasche.Error
import Language.Hasche.Eval
import Language.Hasche.Object
import Prelude hiding (pred)

match ::
  (MonadIO m, MonadThrow m) =>
  Object (EvalT m) ->
  [(Object (EvalT m), Object (EvalT m))] ->
  EvalT m (Object (EvalT m))
match expr arms = do
  obj <- eval expr
  arms' <- for arms \(pat, body) -> do
    parsedPat <- parsePattern pat
    maybeBinder <- matcher parsedPat obj
    pure $ fmap (,body) maybeBinder
  case asum arms' of
    Nothing -> pure undef
    Just (binder, body) -> withNewScope $ binder *> eval body

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

parsePattern ::
  (MonadIO m, MonadThrow m) => Object (EvalT m) -> EvalT m Pattern
parsePattern =
  listify >=> \case
    NonList (Bool b) -> pure $ PBool b
    NonList (Num n) -> pure $ PNum n
    NonList (Str s) -> pure $ PStr s
    NonList (Sym "_") -> pure PIgnore
    NonList (Sym s) -> pure $ PVar s
    NonList _ -> throw ESyntax
    List [Sym "quote", obj] -> parseSExprPattern obj
    List [Sym "quasiquote", obj] -> parseQuasiPattern obj
    List [Sym "?", Sym pred, obj] -> PPred pred <$> parsePattern obj
    List [obj, Sym "..."] -> PRest <$> parsePattern obj
    List objs -> do
      foldrM (\o p -> PCons p <$> parsePattern o) PEmpty objs
    DList objs obj -> do
      pat <- parsePattern obj
      foldrM (\o p -> PCons p <$> parsePattern o) pat objs

parseSExprPattern ::
  (MonadIO m, MonadThrow m) => Object (EvalT m) -> EvalT m Pattern
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
  (MonadIO m, MonadThrow m) => Object (EvalT m) -> EvalT m Pattern
parseQuasiPattern =
  listify >=> \case
    NonList (Bool b) -> pure $ PBool b
    NonList (Num n) -> pure $ PNum n
    NonList (Str s) -> pure $ PStr s
    NonList (Sym s) -> pure $ PSym s
    NonList _ -> throw ESyntax
    List [Sym "unquote", obj] -> parsePattern obj
    List objs -> do
      foldrM (\o p -> PCons p <$> parseQuasiPattern o) PEmpty objs
    DList objs obj -> do
      pat <- parseQuasiPattern obj
      foldrM (\o p -> PCons p <$> parseQuasiPattern o) pat objs

matcher :: (MonadIO m, MonadThrow m) => Pattern -> Object (EvalT m) -> EvalT m (Maybe (EvalT m ()))
matcher (PVar var) obj = pure . Just $ bind var obj
matcher PIgnore _ = pure . Just $ pure ()
matcher PEmpty Empty = pure . Just $ pure ()
matcher (PBool b) (Bool b') = pure $ pure () <$ guard (b == b')
matcher (PNum n) (Num n') = pure $ pure () <$ guard (n == n')
matcher (PStr s) (Str s') = pure $ pure () <$ guard (s == s')
matcher (PSym s) (Sym s') = pure $ pure () <$ guard (s == s')
matcher (PPred pred pat) obj = do
  pred' <- eval =<< sym pred
  t <- apply pred' [obj]
  case t of
    Bool False -> pure Nothing
    _ -> matcher pat obj
matcher (PCons pat1 pat2) (Cons ref1 ref2) = do
  binder1 <- deref ref1 >>= matcher pat1
  binder2 <- deref ref2 >>= matcher pat2
  pure $ liftA2 (*>) binder1 binder2
matcher (PRest pat) obj = do
  listify obj >>= \case
    List objs@(_ : _) ->
      objs
        & traverse (Compose . matcher pat)
        & getCompose
        & fmap (fmap sequence_)
    _ -> pure Nothing
matcher _ _ = pure Nothing
