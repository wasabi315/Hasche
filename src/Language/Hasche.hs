{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Hasche
  ( newInterpreter,
    pretty,
    Object,
    Error,
  )
where

import Control.Exception.Safe
import Data.FileEmbed
import Data.Text (Text)
import Language.Hasche.Builtins
import Language.Hasche.Eval
import Language.Hasche.Eval.Error
import Language.Hasche.Syntax.Reader

newInterpreter :: FilePath -> IO (Text -> IO (Either Error Object))
newInterpreter fp = do
  topEnv <- builtinEnv

  let run txt =
        case readSExprList fp txt of
          Left e -> pure (Left (ReadError e))
          Right prog -> evaluate topEnv prog

  -- load standard library
  res <- run $(makeRelativeToProject "lib/stdlib.scm" >>= embedStringFile)
  case res of
    Left e -> throwString $ "Failed to load standard library: " ++ displayException e
    Right _ -> pure ()

  pure run
