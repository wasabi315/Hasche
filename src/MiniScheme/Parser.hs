{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniScheme.Parser
  ( parseExp,
    ParseError,
  )
where

import Control.Exception.Safe
import Data.Bifunctor
import Data.Text (Text)
import Data.Void
import MiniScheme.AST qualified as AST
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

parseExp :: Text -> Either ParseError AST.Exp
parseExp = first ParseError . parse pExp ""

newtype ParseError = ParseError (ParseErrorBundle Text Void)

instance Show ParseError where
  show (ParseError err) = errorBundlePretty err

instance Exception ParseError

-- The parser type
type Parser = Parsec Void Text

pExp :: Parser AST.Exp
pExp =
  choice
    [ AST.Atom <$> pAtom,
      between (char '(' *> space) (space <* char ')') do
        f : xs <- (:) <$> pExp <*> many (space1 *> pExp)
        pure $ AST.App f xs
    ]

pAtom :: Parser AST.Atom
pAtom =
  choice
    [ AST.Bool True <$ string "#t",
      AST.Bool False <$ string "#f",
      AST.Int <$> Lexer.decimal,
      AST.Id <$> pId
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
      string "procedure?",
      string "not"
    ]
