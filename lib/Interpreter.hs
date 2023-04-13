module Interpreter (eval, run, Value (..)) where

import Data.Text as T
import Data.Time (LocalTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import Parser (Expr (..), parse)

data Value
    = TextValue Text
    | TimeValue LocalTime
    | Void
    deriving stock (Show, Eq)

instance IsString Value where
    fromString = TextValue . T.pack

run :: Text -> Value
run inp = case parse inp of
    Left err -> TextValue $ "error: " <> show err
    Right e -> eval e

eval :: Expr -> Value
eval e = go e Void
  where
    go :: Expr -> Value -> Value
    go (Call fn args) x = callBuiltin fn args x
    go (Pipe c exprs) x = go exprs $ go c x

callBuiltin :: Text -> [Text] -> Value -> Value
callBuiltin "uuid" [] Void = TextValue "123-456"
callBuiltin "strip" [x] (TextValue v) = TextValue $ T.replace x "" v
callBuiltin "parseTime" [x] Void =
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%s%Ez" (T.unpack x) of
        Nothing -> error "bad time format"
        Just v -> TimeValue v
callBuiltin _ _ _ = undefined