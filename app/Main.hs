{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands.Exec qualified as Cmd
import Commands.REPL qualified as Cmd
import Data.Foldable
import Options.Applicative

main :: IO ()
main =
  execParser parserInfo >>= \case
    Repl -> Cmd.repl
    Exec path -> Cmd.exec path

data Command
  = Repl
  | Exec FilePath

parserInfo :: ParserInfo Command
parserInfo =
  withInfo (header "hasche: A Mini-Scheme Interpreter") $
    subparser . fold $
      [ command "repl" . withInfo (progDesc "Start REPL session") $
          pure Repl,
        command "exec" . withInfo (progDesc "Run program") $
          Exec <$> argument str (metavar "[FILE]")
      ]

-- Util

withInfo :: InfoMod a -> Parser a -> ParserInfo a
withInfo m opts = info (helper <*> opts) m
