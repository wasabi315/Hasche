{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module MiniScheme.Interpreter
  ( main,
  )
where

import Data.Text.IO qualified as Text
import MiniScheme.Driver qualified as MS
import System.Exit
import System.IO

main :: FilePath -> IO ExitCode
main path = do
  txt <- Text.readFile path
  interp <- MS.newInterpreter
  interp txt >>= \case
    Right _ -> pure ExitSuccess
    Left err -> ExitFailure 1 <$ hPrint stderr err
