{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasche.Driver
  ( newInterpreter,
    pretty,
    Object,
    Error,
  )
where

import Control.Exception.Safe
import Data.FileEmbed
import Data.Text (Text)
import Hasche.Builtins
import Hasche.Eval
import Hasche.Format
import Hasche.Object hiding (Object)
import Hasche.Object qualified as Obj (Object)
import Hasche.Reader

newInterpreter :: FilePath -> IO (Text -> IO (Either SomeException Object))
newInterpreter fp = do
  topEnv <- childEnv =<< builtinEnv

  let run txt =
        case readSExprList fp txt of
          Left e -> pure (Left $ SomeException (ReadError e))
          Right prog -> do
            catch
              (runEvalM (evalMany topEnv prog) topEnv (pure . Right . Object))
              (pure . Left)

  -- load standard library
  res <- run $(makeRelativeToProject "lib/stdlib.scm" >>= embedStringFile)
  case res of
    Left e -> throwString $ "Failed to load standard library: " ++ displayException e
    Right _ -> pure ()

  pure run

data Object = forall m. Object (Obj.Object m)

pretty :: Object -> IO Text
pretty (Object obj) = write obj
