{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Hasche
  ( exec,
    mkInteractive,
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.FileEmbed
import Data.IORef
import Data.Text qualified as T
import Language.Hasche.Error
import Language.Hasche.Eval
import Language.Hasche.Object
import Language.Hasche.Pretty
import Language.Hasche.Primitives
import Language.Hasche.Reader

runMany ::
  (MonadThrow m, MonadIO m) =>
  FilePath ->
  T.Text ->
  EvalT r m (Object (EvalT r m))
runMany fp = readObjectList fp >=> evalMany

run :: (MonadThrow m, MonadIO m) => T.Text -> EvalT r m (Object (EvalT r m))
run = readObject >=> eval

prepareStdlib :: (MonadThrow m, MonadIO m) => EvalT r m ()
prepareStdlib = void do
  runMany "lib/stdlib.scm" $(makeRelativeToProject "lib/stdlib.scm" >>= embedStringFile)

exec ::
  (MonadIO m, MonadCatch m) =>
  FilePath ->
  T.Text ->
  m (Either Error ())
exec fp src =
  do
    env <- emptyEnv
    runEvalT env do
      preparePrimitives
      prepareStdlib
      _ <- runMany fp src
      pure $ Right ()
    `catch` \e -> pure $ Left e

mkInteractive :: forall m. (MonadIO m, MonadCatch m) => m (T.Text -> m (Either Error T.Text))
mkInteractive = do
  env <- emptyEnv
  initalized <- liftIO $ newIORef False
  pure \input ->
    runEvalT env do
      ini <- liftIO $ readIORef initalized
      unless ini do
        preparePrimitives
        prepareStdlib
        liftIO $ writeIORef initalized True
      obj <- run input
      if objID obj /= undefID @(EvalT _ m)
        then Right <$> display obj
        else pure (Right "")
      `catch` \e -> pure $ Left e
