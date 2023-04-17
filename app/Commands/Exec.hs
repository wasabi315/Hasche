module Commands.Exec (exec) where

import Control.Exception.Safe
import Data.Text.IO qualified as T
import Language.Hasche qualified as Hasche
import System.Exit
import System.IO

exec :: FilePath -> IO ()
exec path = do
  txt <- T.readFile path
  Hasche.exec path txt >>= \case
    Left err -> hPutStrLn stderr (displayException err) *> exitFailure
    Right _ -> exitSuccess
