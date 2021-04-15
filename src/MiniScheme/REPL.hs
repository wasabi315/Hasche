{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MiniScheme.REPL
  ( main,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.Driver qualified as MS
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  Text.putStrLn header
  repl

repl :: IO ()
repl = do
  Text.putStr prompt
  txt <- Text.getLine
  if
      | Text.null txt -> repl
      | txt == ":help" || txt == ":?" -> Text.putStrLn help *> repl
      | txt == ":quit" || txt == ":q" -> pure ()
      | otherwise ->
        MS.runInterpreter txt >>= \case
          Left err -> print err *> repl
          Right v -> print v *> repl

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
