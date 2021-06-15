{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Hasche.Reader
  ( readSExprList,
    readSExpr,
    readSNum,
    ReadError,
  )
where

import Control.Monad
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Language.Hasche.SExpr
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder

type Reader = Parsec Void Text

type ReadError = ParseErrorBundle Text Void

readSExprList :: FilePath -> Text -> Either ReadError [SExpr]
readSExprList = parse (space *> many expr <* eof)

readSExpr :: Text -> Either ReadError SExpr
readSExpr = parse (space *> expr <* eof) ""

readSNum :: Text -> Maybe SExpr
readSNum = parseMaybe num

expr :: Reader SExpr
expr =
  choice
    [ atom,
      quoted,
      pairs
    ]

quoted :: Reader SExpr
quoted = do
  q <-
    choice
      [ SQuote <$ char '\'',
        SQQ <$ char '`',
        SUQS <$ string ",@",
        SUQ <$ char ','
      ]
  q <$> expr

pairs :: Reader SExpr
pairs = between (lexeme lparen) (lexeme rparen) do
  e : es <- some expr
  choice
    [ SList (e NE.:| es) <$ lookAhead rparen,
      SDList (e NE.:| es) <$> (symbol "." *> expr)
    ]

atom :: Reader SExpr
atom =
  choice
    [ SEmpty <$ symbol "()",
      SBool True <$ symbol "#true",
      SBool True <$ symbol "#t",
      SBool False <$ symbol "#false",
      SBool False <$ symbol "#f",
      try (lexeme num),
      lexeme str,
      lexeme ident
    ]

num :: Reader SExpr
num = SNum <$> L.signed (pure ()) L.decimal

str :: Reader SExpr
str = SStr . T.concat <$> between (char '"') (char '"') (many str')
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

ident :: Reader SExpr
ident = try do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "."
    then parseError (err o (utoks x))
    else pure $ SSym x

space :: Reader ()
space =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockCommentNested "#|" "|#")

lexeme :: Reader a -> Reader a
lexeme = L.lexeme space

symbol :: Text -> Reader Text
symbol = L.symbol space

lparen, rparen :: Reader ()
lparen = void $ satisfy (\c -> c == '(' || c == '[')
rparen = void $ satisfy (\c -> c == ')' || c == ']')
