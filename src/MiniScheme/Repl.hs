{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Repl where

import Data.Function
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.Interpreter qualified as MS
import MiniScheme.Parser qualified as MS
import System.IO

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  Text.putStrLn header

  fix \loop -> do
    Text.putStr prompt
    txt <- Text.getLine
    if
        | Text.null txt ->
          loop
        | txt == ":help" || txt == ":?" ->
          Text.putStrLn help >> loop
        | txt == ":quit" || txt == ":q" ->
          pure ()
        | otherwise ->
          case MS.parseExp txt >>= MS.interpret of
            Right e -> print e >> loop
            Left err -> putStrLn err >> loop

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
