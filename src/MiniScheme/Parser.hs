{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Parser where

import Data.Bifunctor
import Data.Text (Text)
import Data.Void
import MiniScheme.AST qualified as AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- The parser type
type Parser = Parsec Void Text

parseExp :: Text -> Either String AST.Exp
parseExp = first errorBundlePretty . parse pExp ""

pExp :: Parser AST.Exp
pExp =
  choice
    [ AST.Const <$> pConst,
      AST.Id <$> pId,
      between (char '(' *> space) (space <* char ')') do
        f : xs <- (:) <$> pExp <*> many (space1 *> pExp)
        pure $ AST.App f xs
    ]

pId :: Parser AST.Id
pId =
  choice
    [ "+" <$ char '+',
      "-" <$ char '-',
      "*" <$ char '*',
      "/" <$ char '/',
      "=" <$ char '=',
      string "<=",
      "<" <$ char '<',
      string ">=",
      ">" <$ char '>',
      string "number?",
      string "boolean?",
      string "not"
    ]

pConst :: Parser AST.Const
pConst =
  choice
    [ AST.Bool True <$ string "#t",
      AST.Bool False <$ string "#f",
      AST.Int <$> Lexer.decimal
    ]
