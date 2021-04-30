{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Exception
import Data.Foldable
import Data.Function
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hasche.Driver as H
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
  txt <- T.readFile path
  interp <- H.newInterpreter path
  interp txt >>= \case
    Right _ -> exitSuccess
    Left err -> hPrint stderr err *> exitFailure

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn headerText

  interp <- H.newInterpreter "<interactive>"

  fix \loop -> do
    putStr promptText
    txt <- getLine
    if
        | txt == "" -> loop
        | txt == ":help" || txt == ":?" -> putStrLn helpText >> loop
        | txt == ":quit" || txt == ":q" -> pure ()
        | otherwise ->
          do
            res <- interp (T.pack txt)
            case res of
              Left err -> putStrLn (displayException err)
              Right obj -> H.display obj >>= T.putStrLn
            loop

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
