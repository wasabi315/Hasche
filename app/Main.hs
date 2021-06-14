{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hasche.Driver
import Options.Applicative hiding (help)
import System.Console.Haskeline
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

withInfo :: InfoMod a -> Parser a -> ParserInfo a
withInfo m opts = info (helper <*> opts) m

parserInfo :: ParserInfo Command
parserInfo =
  withInfo (header "hasche: A Mini-Scheme Interpreter") $
    subparser . fold $
      [ command "repl" . withInfo (progDesc "Start REPL session") $
          pure Repl,
        command "exec" . withInfo (progDesc "Run program") $
          Exec <$> argument str (metavar "[FILE]")
      ]

exec :: FilePath -> IO ()
exec path = runInputT defaultSettings do
  txt <- liftIO $ T.readFile path
  interpret <- liftIO $ newInterpreter path

  withInterrupt . handleInterrupt (outputStrLn "Interrupted") $ do
    liftIO (interpret txt) >>= \case
      Left err -> liftIO $ hPutStrLn stderr (displayException err) >> exitFailure
      Right _ -> liftIO exitSuccess

repl :: IO ()
repl = runInputT defaultSettings do
  outputStrLn headerText
  interpret <- liftIO $ newInterpreter "<interactive>"

  withInterrupt . forever . handleInterrupt (outputStrLn "Interrupted") $ do
    minput <- getInputLine promptText
    case minput of
      Nothing -> pure ()
      Just (':' : cmd)
        | Just m <- lookup cmd commands -> m
        | otherwise -> outputStrLn $ "Unknown command: " ++ cmd
      Just txt -> do
        result <- liftIO $ interpret (T.pack txt)
        case result of
          Left err -> outputStrLn (displayException err)
          Right obj -> liftIO (pretty obj) >>= outputStrLn . T.unpack

headerText :: String
headerText =
  "Welcome to the Hasche REPL!\n\
  \enter :? for help\n"

helpText :: String
helpText =
  "Commands available from the prompt:\n\
  \\n\
  \<expression>    evaluate/run <expression>\n\
  \:help, :?       display this help text\n\
  \:quit, :q       exit REPL\n"

promptText :: String
promptText = "hasche> "

-- Commands

commands :: [(String, InputT IO ())]
commands =
  [ ("help", help),
    ("?", help),
    ("quit", quit),
    ("q", quit)
  ]

help :: InputT IO ()
help = outputStrLn helpText

quit :: InputT IO ()
quit = liftIO exitSuccess
