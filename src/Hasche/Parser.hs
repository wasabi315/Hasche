{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasche.Parser
  ( parseExpr,
    parseNum,
    ParseError,
  )
where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Hasche.Object qualified as Obj
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder

type Parser = ParsecT Void Text IO

type ParseError = ParseErrorBundle Text Void

parseExpr :: FilePath -> Text -> IO (Either ParseError [Obj.ObjRef m])
parseExpr = runParserT (space *> many expr <* eof)

parseNum :: Text -> IO (Maybe (Obj.ObjRef m))
parseNum t =
  either (const Nothing) Just <$> runParserT (num <* eof) "" t

expr :: Parser (Obj.ObjRef m)
expr =
  choice
    [ atom,
      quoted,
      pairs
    ]

quoted :: Parser (Obj.ObjRef m)
quoted = do
  _ <- char '\''
  e <- expr
  q <- Obj.sym "quote"
  Obj.cons q =<< Obj.cons e Obj.empty

pairs :: Parser (Obj.ObjRef m)
pairs = between (symbol "(") (symbol ")") do
  es <- some expr
  e <-
    choice
      [ Obj.empty <$ lookAhead (char ')'),
        symbol "." *> expr
      ]
  foldrM Obj.cons e es

atom :: Parser (Obj.ObjRef m)
atom =
  choice
    [ Obj.empty <$ symbol "()",
      Obj.true <$ symbol "#true",
      Obj.true <$ symbol "#t",
      Obj.false <$ symbol "#false",
      Obj.false <$ symbol "#f",
      try (lexeme num),
      lexeme str,
      lexeme ident
    ]

num :: Parser (Obj.ObjRef m)
num = L.signed (pure ()) L.decimal >>= Obj.num

str :: Parser (Obj.ObjRef m)
str =
  between (char '"') (char '"') (Text.concat <$!> many str')
    >>= Obj.str
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

ident :: Parser (Obj.ObjRef m)
ident = try do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "."
    then parseError (err o (utoks x))
    else Obj.sym x

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
