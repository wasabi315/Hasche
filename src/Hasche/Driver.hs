{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasche.Driver
  ( newInterpreter,
    display,
    Error,
  )
where

import Control.Exception.Safe
import Data.Text (Text)
import Data.Text.IO qualified as T
import Hasche.Builtins
import Hasche.Eval
import Hasche.Format qualified as Fmt
import Hasche.Object
import Hasche.Reader

newInterpreter :: FilePath -> IO (Text -> IO (Either Error SomeObj))
newInterpreter fp = do
  topEnv <- childEnv =<< builtinEnv

  let run txt =
        case readSExprList fp txt of
          Left e -> pure (Left (ReadError e))
          Right prog -> do
            catch
              (runEvalM (evalMany topEnv prog) topEnv (pure . Right . Obj))
              (pure . Left)

  -- load standard library
  res <- T.readFile "lib/stdlib.scm" >>= run
  case res of
    Left e -> throwString $ "Failed to load standard library: " ++ displayException e
    Right _ -> pure ()

  pure run

data SomeObj = forall m. Obj (Object m)

display :: SomeObj -> IO Text
display (Obj obj) = Fmt.display obj
