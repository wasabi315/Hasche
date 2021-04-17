{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.Driver as MS
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [] -> repl *> exitSuccess
    [path] -> interpFile path
    _ -> Text.hPutStrLn stderr (usage (Text.pack prog)) *> exitFailure

usage :: Text -> Text
usage progName = Text.unlines ["Usage:", progName, progName <> " [FILE]"]

interpFile :: FilePath -> IO ()
interpFile path = do
  txt <- Text.readFile path
  interp <- MS.newInterpreter
  interp txt >>= \case
    Right _ -> exitSuccess
    Left err -> hPrint stderr err *> exitFailure

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  Text.putStrLn header

  interp <- MS.newInterpreter

  fix \loop -> do
    Text.putStr prompt
    txt <- Text.getLine
    if
        | Text.null txt -> loop
        | txt == ":help" || txt == ":?" -> Text.putStrLn help >> loop
        | txt == ":quit" || txt == ":q" -> pure ()
        | otherwise -> interp txt >>= either print print >> loop

  exitSuccess

header :: Text
header =
  "Welcome to the Mini-Scheme REPL!\n\
  \enter :? for help\n"

help :: Text
help =
  "Commands available from the prompt:\n\
  \\n\
  \<expression>    evaluate/run <expression>\n\
  \:help, :?       display this help text\n\
  \:quit, :q       exit REPL\n"

prompt :: Text
prompt = "minischeme> "
