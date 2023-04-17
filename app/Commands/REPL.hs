{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Commands.REPL
  ( repl,
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Text qualified as T
import Language.Hasche qualified as Hasche
import System.Console.Haskeline
import System.Exit
import System.IO

repl :: IO ()
repl = runInputT defaultSettings do
  liftIO $ hSetBuffering stdout NoBuffering
  outputStrLn headerText
  interpret <- Hasche.mkInteractive

  withInterrupt . forever . handleInterrupt (outputStrLn "Interrupted") $ do
    minput <- getInputLine promptText
    case minput of
      Nothing -> pure ()
      Just (':' : cmd)
        | Just m <- lookup cmd commands -> m
        | otherwise -> outputStrLn $ "Unknown command: " ++ cmd
      Just txt -> do
        result <- interpret (T.pack txt)
        case result of
          Left err -> outputStrLn (displayException err)
          Right objTxt -> outputStrLn $ T.unpack objTxt

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

-- REPL Commands

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
