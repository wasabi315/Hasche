module Language.Hasche.Reader
  ( readObject,
    readObjectList,
    readNum,
    ReadError,
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

type Reader = ParsecT Void Text Eval

newtype ReadError = ReadError (ParseErrorBundle Text Void)
  deriving newtype (Show, Exception)

readObjectList :: FilePath -> Text -> Eval [Object]
readObjectList path input = do
  res <- runParserT (space *> many pExpr <* eof) path input
  either (throw . Error . ReadError) pure res

readObject :: Text -> Eval Object
readObject input = do
  res <- runParserT (space *> pExpr <* eof) "" input
  either (throw . Error . ReadError) pure res

readNum :: Text -> Eval Object
readNum input = do
  res <- runParserT pNum "" input
  either (throw . Error . ReadError) pure res

pExpr :: Reader Object
pExpr =
  choice
    [ pAtom,
      pQuoted,
      pPairs
    ]

pQuoted :: Reader Object
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

pPairs :: Reader Object
pPairs = between (lexeme lparen) (lexeme rparen) do
  e : es <- some pExpr
  choice
    [ lift (list (e NE.:| es)) <* lookAhead rparen,
      lift . dlist (e NE.:| es) =<< (symbol "." *> pExpr)
    ]

pAtom :: Reader Object
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

pNum :: Reader Object
pNum = lift . num =<< L.signed (pure ()) L.decimal

pStr :: Reader Object
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

pIdent :: Reader Object
pIdent = try do
  o <- getOffset
  x <- takeWhile1P Nothing \c ->
    isAlphaNum c
      || c `elem` ("!$%&*+-./<=>?@^_" :: String)
  if x == "."
    then parseError (err o (utoks x))
    else lift $ sym x

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
