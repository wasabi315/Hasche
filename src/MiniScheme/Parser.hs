{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Parser
  ( parseProg,
    parseNum,
    ParseError,
  )
where

import Control.Exception.Safe (Exception)
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import MiniScheme.AST qualified as AST
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder
import Prelude hiding (exp)

parseProg :: Text -> Either ParseError [AST.Prog]
parseProg = first ParseError . parse (space *> some prog <* eof) ""

-- for string->number
parseNum :: Text -> Maybe AST.Number
parseNum = parseMaybe (num' <* eof)

newtype ParseError = ParseError (ParseErrorBundle Text Void)

instance Show ParseError where
  show (ParseError e) = errorBundlePretty e

instance Exception ParseError

-- The parser type
type Parser = Parsec Void Text

prog :: Parser AST.Prog
prog =
  choice
    [ AST.Exp <$> atomicExp,
      parens $ (AST.Def <$> define) <|> (AST.Exp <$> nonAtomicExp)
    ]

define :: Parser AST.Def
define = do
  _ <- symbol "define"
  choice
    [ do
        x <- ident
        e <- exp
        pure $! AST.Const x e,
      do
        f : xs <- parens (some ident)
        b <- body
        pure $! AST.Const f (AST.Lam xs b)
    ]

exp :: Parser AST.Exp
exp = atomicExp <|> parens nonAtomicExp

atomicExp :: Parser AST.Exp
atomicExp = AST.Atom <$> atom

nonAtomicExp :: Parser AST.Exp
nonAtomicExp =
  choice
    [ do
        _ <- symbol "lambda"
        xs <- parens (many ident)
        b <- body
        pure $! AST.Lam xs b,
      do
        _ <- symbol "let*"
        bs <- parens . many . parens $ (,) <$> ident <*> exp
        b <- body
        pure $! AST.LetA bs b,
      do
        _ <- symbol "let"
        bs <- parens . many . parens $ (,) <$> ident <*> exp
        b <- body
        pure $! AST.Let bs b,
      do
        _ <- symbol "set!"
        i <- ident
        e <- exp
        pure $! AST.Set i e,
      do
        _ <- symbol "if"
        p <- exp
        t <- exp
        e <- optional exp
        pure $! AST.If p t e,
      do
        f : xs <- some exp
        pure $! AST.App f xs
    ]

body :: Parser AST.Body
body = do
  ds <- many (try (parens define))
  e : es <- some exp
  pure $! AST.Body ds (e NE.:| es)

atom :: Parser AST.Atom
atom =
  choice
    [ AST.Empty <$ symbol "()",
      AST.Bool True <$ symbol "#t",
      AST.Bool False <$ symbol "#f",
      try $ AST.Num <$> num,
      AST.Str <$> str,
      AST.Id <$> ident
    ]

num :: Parser AST.Number
num = lexeme num'

num' :: Parser AST.Number
num' = do
  f <- option id $ (id <$ char '+') <|> (negate <$ char '-')
  n <- L.decimal
  pure $! f n

str :: Parser Text
str = lexeme $ between (char '"') (char '"') (Text.concat <$!> many str')
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

ident :: Parser AST.Id
ident = lexeme do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "." || x `Set.member` keywords
    then parseError (err o (utoks x))
    else pure x

keywords :: Set Text
keywords =
  Set.fromList
    [ "define",
      "lambda",
      "if",
      "set!",
      "let",
      "let*",
      "letrec"
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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
