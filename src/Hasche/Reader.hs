{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasche.Reader
  ( readSExprList,
    readSExpr,
    readSNum,
    ReadError,
  )
where

import Control.Monad
import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Hasche.SExpr
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder

type Parser = Parsec Void Text

type ReadError = ParseErrorBundle Text Void

readSExprList :: FilePath -> Text -> Either ReadError [SExpr]
readSExprList = parse (space *> many expr <* eof)

readSExpr :: Text -> Either ReadError SExpr
readSExpr = parse (space *> expr <* eof) ""

readSNum :: Text -> Maybe SExpr
readSNum = parseMaybe num

expr :: Parser SExpr
expr =
  choice
    [ atom,
      quoted,
      pairs
    ]

quoted :: Parser SExpr
quoted = do
  _ <- char '\''
  e <- expr
  pure $! SList [SSym "quote", e] Nothing

pairs :: Parser SExpr
pairs = between (symbol "(") (symbol ")") do
  es <- some expr
  choice
    [ SList es Nothing <$ lookAhead (char ')'),
      SList es . Just <$!> (symbol "." *> expr)
    ]

atom :: Parser SExpr
atom =
  choice
    [ SList [] Nothing <$ symbol "()",
      SBool True <$ symbol "#true",
      SBool True <$ symbol "#t",
      SBool False <$ symbol "#false",
      SBool False <$ symbol "#f",
      try (lexeme num),
      lexeme str,
      lexeme ident
    ]

num :: Parser SExpr
num = SNum <$!> L.signed (pure ()) L.decimal

str :: Parser SExpr
str = SStr <$!> between (char '"') (char '"') (T.concat <$!> many str')
  where
    str' =
      choice
        [ do
            _ <- char '\\'
            choice
              [ "\n" <$ string "n",
                "\r" <$ string "r",
                "\t" <$ string "t",
                "\"" <$ string "\"",
                "\\" <$ string "\\"
              ],
          takeWhile1P Nothing \c -> c /= '\\' && c /= '"'
        ]

ident :: Parser SExpr
ident = try do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "."
    then parseError (err o (utoks x))
    else pure $! SSym x

space :: Parser ()
space =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockCommentNested "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space
