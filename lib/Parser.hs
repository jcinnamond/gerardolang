module Parser (parse, Expr (..)) where

import Data.Text qualified as T
import Text.Megaparsec (MonadParsec (eof, try), ParseErrorBundle, Parsec, runParser)
import Text.Megaparsec.Char (alphaNumChar, hspace, string)

data Expr
    = Call Text [Text]
    | Pipe Expr Expr
    deriving (Show, Eq)

type Parser = Parsec Void Text

parse :: Text -> Either (ParseErrorBundle Text Void) Expr
parse = runParser parseExpr ""

parseExpr :: Parser Expr
parseExpr = (try parsePipe <|> parseCall) <* eof

parsePipe :: Parser Expr
parsePipe = Pipe <$> parseCall <* parseSeparator <*> parseExpr

parseSeparator :: Parser ()
parseSeparator = hspace *> string "|>" *> hspace

parseCall :: Parser Expr
parseCall = flip Call [] <$> parseIdentifier

parseIdentifier :: Parser Text
parseIdentifier = T.pack <$> many alphaNumChar