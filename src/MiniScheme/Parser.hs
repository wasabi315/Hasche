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
import Data.Char
import Data.Text (Text)
import Data.Void
import MiniScheme.AST qualified as AST
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Text.Megaparsec.Error.Builder

parseExp :: Text -> Either ParseError AST.Exp
parseExp = first ParseError . parse pExp ""

newtype ParseError = ParseError (ParseErrorBundle Text Void)

instance Show ParseError where
  show (ParseError e) = errorBundlePretty e

instance Exception ParseError

-- The parser type
type Parser = Parsec Void Text

pExp :: Parser AST.Exp
pExp =
  choice
    [ AST.Atom <$> pAtom,
      between (char '(' *> space) (char ')') do
        f : xs <- pExp `sepEndBy1` space1
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
pId = do
  o <- getOffset
  i <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if i == "."
    then parseError (err o (utok '.'))
    else pure i
