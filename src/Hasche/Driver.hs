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
import Hasche.Builtins
import Hasche.Eval
import Hasche.Format qualified as Fmt
import Hasche.Object
import Hasche.Reader

newInterpreter :: FilePath -> IO (Text -> IO (Either Error SomeObj))
newInterpreter fp = do
  env <- builtinEnv

  let run txt =
        case readSExprList fp txt of
          Left err -> pure (Left (ReadError err))
          Right prog -> do
            catch
              (runEvalM (evalMany env prog) (pure . Right . Obj))
              (pure . Left)

  -- load standard library
  _ <- run "(load \"lib/stdlib.scm\")"

  pure run

data SomeObj = forall m. Obj (ObjRef m)

display :: SomeObj -> IO Text
display (Obj obj) = Fmt.display obj
