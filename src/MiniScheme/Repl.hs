{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Repl where

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniScheme.Interpreter qualified as MS
import MiniScheme.Parser qualified as MS
import System.IO

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to the Mini-Scheme REPL!"

  fix \loop -> do
    putStr "ms> "
    txt <- Text.getLine
    if
        | Text.null txt -> loop
        | txt == "exit" -> pure ()
        | otherwise ->
          case MS.parseExp txt >>= MS.interpret of
            Right e -> print e >> loop
            Left err -> putStrLn err >> loop
