{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Data.Foldable
import Data.Function
import Data.String
import Data.Text.IO qualified as Text
import MiniScheme.Driver as MS
import Options.Applicative
import System.Exit
import System.IO

main :: IO ()
main = do
  execParser parserInfo >>= \case
    Repl -> repl
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
  interp <- MS.newInterpreter path
  interp txt >>= \case
    Right _ -> exitSuccess
    Left err -> hPrint stderr err *> exitFailure

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn headerText

  interp <- MS.newInterpreter "<interactive>"

  fix \loop -> do
    putStr promptText
    txt <- getLine
    if
        | txt == "" -> loop
        | txt == ":help" || txt == ":?" -> putStrLn helpText >> loop
        | txt == ":quit" || txt == ":q" -> pure ()
        | otherwise ->
          interp (fromString txt)
            >>= either print (pretty >=> putStrLn)
            >> loop

  exitSuccess

headerText :: String
headerText =
  "Welcome to the Mini-Scheme REPL!\n\
  \enter :? for help\n"

helpText :: String
helpText =
  "Commands available from the prompt:\n\
  \\n\
  \<expression>    evaluate/run <expression>\n\
  \:help, :?       display this help text\n\
  \:quit, :q       exit REPL\n"

promptText :: String
promptText = "minischeme> "
