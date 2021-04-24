{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasche.Parser
  ( Parser,
    ParseError,
    parseExpr,
    parseNum,
  )
where

import Control.Monad
import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Hasche.AST qualified as AST
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder

type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void

parseExpr :: FilePath -> Text -> Either ParseError [AST.Expr]
parseExpr = parse (space *> many expr <* eof)

parseNum :: Text -> Maybe Integer
parseNum = parseMaybe (num <* eof)

expr :: Parser AST.Expr
expr =
  choice
    [ AST.Atom <$!> atom,
      quoted,
      pairs
    ]

quoted :: Parser AST.Expr
quoted = do
  _ <- char '\''
  e <- expr
  pure
    $! AST.Atom (AST.Ident "quote")
    `AST.Pair` e
    `AST.Pair` AST.Atom AST.Empty

pairs :: Parser AST.Expr
pairs = between (symbol "(") (symbol ")") do
  es <- some expr
  e <-
    choice
      [ AST.Atom AST.Empty <$ lookAhead (char ')'),
        symbol "." *> expr
      ]
  pure $! foldr AST.Pair e es

atom :: Parser AST.Atom
atom =
  choice
    [ AST.Empty <$ symbol "()",
      AST.Bool True <$ symbol "#t",
      AST.Bool False <$ symbol "#f",
      AST.Num <$!> lexeme num,
      AST.Str <$!> lexeme str,
      AST.Ident <$!> lexeme ident
    ]

num :: Parser Integer
num = L.signed (pure ()) L.decimal

str :: Parser Text
str = between (char '"') (char '"') (Text.concat <$!> many str')
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

ident :: Parser Text
ident = try $ do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "."
    then parseError (err o (utoks x))
    else pure x

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
