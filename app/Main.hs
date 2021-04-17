{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.Interpreter qualified as Interp
import MiniScheme.REPL qualified as REPL
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [] -> REPL.main *> exitSuccess
    [path] -> Interp.main path >>= exitWith
    _ -> Text.hPutStrLn stderr (usage (Text.pack prog)) *> exitFailure

usage :: Text -> Text
usage progName = Text.unlines ["Usage", progName, progName <> " [FILE]"]
