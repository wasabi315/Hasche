{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable
import Data.Function
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.Driver as MS
import Options.Applicative
import System.Exit
import System.IO

main :: IO ()
main = do
  execParser parserInfo >>= \case
    Repl -> repl *> exitSuccess
    Exec path -> exec path

data Command
  = Repl
  | Exec FilePath

withInfo :: String -> Parser a -> ParserInfo a
withInfo desc opts = info (helper <*> opts) (progDesc desc)

parserInfo :: ParserInfo Command
parserInfo =
  withInfo "A Mini-Scheme Interpreter" $
    subparser . fold $
      [ command "repl" . withInfo "Start REPL session" $
          pure Repl,
        command "exec" . withInfo "Run program" $
          Exec <$> argument str (metavar "[FILE]")
      ]

exec :: FilePath -> IO ()
exec path = do
  txt <- Text.readFile path
  interp <- MS.newInterpreter
  interp txt >>= \case
    Right _ -> exitSuccess
    Left err -> hPrint stderr err *> exitFailure

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  Text.putStrLn headerText

  interp <- MS.newInterpreter

  fix \loop -> do
    Text.putStr promptText
    txt <- Text.getLine
    if
        | Text.null txt -> loop
        | txt == ":help" || txt == ":?" -> Text.putStrLn helpText >> loop
        | txt == ":quit" || txt == ":q" -> pure ()
        | otherwise -> interp txt >>= either print print >> loop

  exitSuccess

headerText :: Text
headerText =
  "Welcome to the Mini-Scheme REPL!\n\
  \enter :? for help\n"

helpText :: Text
helpText =
  "Commands available from the prompt:\n\
  \\n\
  \<expression>    evaluate/run <expression>\n\
  \:help, :?       display this help text\n\
  \:quit, :q       exit REPL\n"

promptText :: Text
promptText = "minischeme> "
