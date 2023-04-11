module Language.Hasche.Parser
  ( parseObject,
    parseObjectList,
    parseNum,
    ParseError,
  )
where

import Control.Exception.Safe (Exception, throw)
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Language.Hasche.Types
import Text.Megaparsec hiding (ParseError, empty)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error.Builder

type Parser = ParsecT Void Text Eval

newtype ParseError = ParseError (ParseErrorBundle Text Void)
  deriving newtype (Show, Exception)

parseObjectList :: FilePath -> Text -> Eval [Object]
parseObjectList path input = do
  res <- runParserT (space *> many pExpr <* eof) path input
  either (throw . Error . ParseError) pure res

parseObject :: Text -> Eval Object
parseObject input = do
  res <- runParserT (space *> pExpr <* eof) "" input
  either (throw . Error . ParseError) pure res

parseNum :: Text -> Eval Object
parseNum input = do
  res <- runParserT pNum "" input
  either (throw . Error . ParseError) pure res

pExpr :: Parser Object
pExpr =
  choice
    [ pAtom,
      pQuoted,
      pPairs
    ]

pQuoted :: Parser Object
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
    q s o = list . (NE.:| [o]) =<< sym s

pPairs :: Parser Object
pPairs = between (lexeme lparen) (lexeme rparen) do
  e : es <- some pExpr
  choice
    [ lift (list (e NE.:| es)) <* lookAhead rparen,
      lift . dlist (e NE.:| es) =<< (symbol "." *> pExpr)
    ]

pAtom :: Parser Object
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

pNum :: Parser Object
pNum = lift . num =<< L.signed (pure ()) L.decimal

pStr :: Parser Object
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

pIdent :: Parser Object
pIdent = try do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "."
    then parseError (err o (utoks x))
    else lift $ sym x

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

lparen, rparen :: Parser ()
lparen = void $ satisfy (\c -> c == '(' || c == '[')
rparen = void $ satisfy (\c -> c == ')' || c == ']')
