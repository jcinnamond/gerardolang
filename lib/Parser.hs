module Parser (parse, Expr (..)) where

import Data.Text qualified as T
import Text.Megaparsec (MonadParsec (eof, try), Parsec, runParser, satisfy, sepBy)
import Text.Megaparsec.Char (alphaNumChar, char, hspace, space, string)

data Expr
    = Call Text [Text]
    | Pipe Expr Expr
    deriving (Show, Eq)

type Parser = Parsec Void Text

parse :: Text -> Either Text Expr
parse inp = case runParser parseExpr "" inp of
    Left err -> Left $ show err
    Right v -> pure v

parseExpr :: Parser Expr
parseExpr = (try parsePipe <|> parseCall) <* space <* eof

parsePipe :: Parser Expr
parsePipe = Pipe <$> parseCall <* parseSeparator <*> parseExpr

parseSeparator :: Parser ()
parseSeparator = hspace *> string "|>" *> hspace

parseCall :: Parser Expr
parseCall = do
    identifier <- parseIdentifier <* hspace
    Call identifier <$> parseArgs

parseArgs :: Parser [Text]
parseArgs = stringLiteral `sepBy` comma

comma :: Parser Char
comma = char ','

stringLiteral :: Parser Text
stringLiteral = char '"' *> notAQuote <* char '"'

notAQuote :: Parser Text
notAQuote = T.pack <$> many (satisfy (/= '"'))

parseIdentifier :: Parser Text
parseIdentifier = T.pack <$> many alphaNumChar