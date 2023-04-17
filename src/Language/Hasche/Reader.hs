{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Hasche.Reader
  ( readObject,
    readObjectList,
    readNum,
  )
where

import Control.Exception.Safe hiding (try)
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Void
import Language.Hasche.Error
import Language.Hasche.Eval
import Language.Hasche.Object
import Text.Megaparsec hiding (ParseError, empty)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder

type ReadT m = ParsecT Void T.Text (EvalT m)

readObjectList ::
  (MonadThrow m, MonadIO m) => FilePath -> T.Text -> EvalT m [Object (EvalT m)]
readObjectList path input = do
  res <- runParserT (space *> many pExpr <* eof) path input
  either (throw . ERead) pure res

readObject :: (MonadThrow m, MonadIO m) => T.Text -> EvalT m (Object (EvalT m))
readObject input = do
  res <- runParserT (space *> pExpr <* eof) "" input
  either (throw . ERead) pure res

readNum :: (MonadThrow m, MonadIO m) => T.Text -> EvalT m (Object (EvalT m))
readNum input = do
  res <- runParserT pNum "" input
  either (throw . ERead) pure res

pExpr :: MonadIO m => ReadT m (Object (EvalT m))
pExpr =
  choice
    [ pAtom,
      pQuoted,
      pPairs
    ]

pQuoted :: MonadIO m => ReadT m (Object (EvalT m))
pQuoted = do
  f <-
    choice
      [ q "quote" <$ char '\'',
        q "quasiquote" <$ char '`',
        q "unquote-splicing" <$ string ",@",
        q "unquote" <$ char ','
      ]
  lift . f =<< pExpr
  where
    q s o = list . (: [o]) =<< sym s

pPairs :: MonadIO m => ReadT m (Object (EvalT m))
pPairs = between (lexeme lparen) (lexeme rparen) do
  e : es <- some pExpr
  choice
    [ lift (list (e : es)) <* lookAhead rparen,
      lift . dlist (e NE.:| es) =<< (symbol "." *> pExpr)
    ]

pAtom :: MonadIO m => ReadT m (Object (EvalT m))
pAtom =
  choice
    [ empty <$ symbol "()",
      true <$ symbol "#true",
      true <$ symbol "#t",
      false <$ symbol "#false",
      false <$ symbol "#f",
      try (lexeme pNum),
      lexeme pStr,
      lexeme pIdent
    ]

pNum :: MonadIO m => ReadT m (Object (EvalT m))
pNum = lift . num =<< L.signed (pure ()) L.decimal

pStr :: MonadIO m => ReadT m (Object (EvalT m))
pStr = lift . str . T.concat =<< between (char '"') (char '"') (many str')
  where
    str' =
      choice
        [ do
            _ <- char '\\'
            choice
              [ "\n" <$ char 'n',
                "\r" <$ char 'r',
                "\t" <$ char 't',
                "\"" <$ char '"',
                "\\" <$ char '\\'
              ],
          takeWhile1P Nothing \c -> c /= '\\' && c /= '"'
        ]

pIdent :: MonadIO m => ReadT m (Object (EvalT m))
pIdent = try do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "."
    then parseError (err o (utoks x))
    else lift $ sym x

space :: ReadT m ()
space =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockCommentNested "#|" "|#")

lexeme :: ReadT m a -> ReadT m a
lexeme = L.lexeme space

symbol :: T.Text -> ReadT m T.Text
symbol = L.symbol space

lparen, rparen :: ReadT m ()
lparen = void $ satisfy (\c -> c == '(' || c == '[')
rparen = void $ satisfy (\c -> c == ')' || c == ']')
