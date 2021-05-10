{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (displayException)
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Text.IO qualified as T
import Hasche.Driver
import Options.Applicative
import System.Exit
import System.IO
import System.Posix.Signals

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
exec path = do
  txt <- T.readFile path
  interpret <- newInterpreter path
  interruptible <- newInterruptibleBy keyboardSignal

  interruptible (interpret txt) >>= \case
    Nothing -> hPutStrLn stderr "Interrupted" *> exitFailure
    Just (Right _) -> exitSuccess
    Just (Left err) -> hPrint stderr err *> exitFailure

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  interruptible <- newInterruptibleBy keyboardSignal

  putStrLn headerText

  interpret <- newInterpreter "<interactive>"

  fix \loop -> do
    putStr promptText
    txt <- T.getLine
    if
        | txt == "" -> loop
        | txt == ":help" || txt == ":?" -> putStrLn helpText >> loop
        | txt == ":quit" || txt == ":q" -> pure ()
        | otherwise ->
          do
            interruptible (interpret txt) >>= \case
              Nothing -> putStrLn "Interrupted"
              Just (Left err) -> putStrLn (displayException err)
              Just (Right obj) -> pretty obj >>= T.putStrLn
            loop

  exitSuccess

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

newInterruptibleBy :: Signal -> IO (IO a -> IO (Maybe a))
newInterruptibleBy sig = do
  sigInvoked <- newEmptyMVar
  active <- newMVar False
  let handler = do
        p <- readMVar active
        when p (putMVar sigInvoked ())
  _ <- installHandler sig (Catch handler) Nothing

  pure \ma -> do
    modifyMVar_ active (const $ pure True)
    ea <- race (takeMVar sigInvoked) ma
    modifyMVar_ active (const $ pure False)
    case ea of
      Left _ -> pure Nothing
      Right a -> pure (Just a)
