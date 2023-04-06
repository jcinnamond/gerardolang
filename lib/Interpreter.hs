module Interpreter (eval, run) where

import Data.Text as T
import Parser (Expr (..), parse)

run :: Text -> Text
run inp = case parse inp of
    Left err -> "error: " <> show err
    Right e -> eval e

eval :: Expr -> Text
eval e = go e ""
  where
    go :: Expr -> Text -> Text
    go (Call fn args) x = callBuiltin fn args x
    go (Pipe c exprs) x = go exprs $ go c x

callBuiltin :: Text -> [Text] -> Text -> Text
callBuiltin "uuid" [] = const "123-456"
callBuiltin "strip" [x] = T.replace x ""
callBuiltin _ _ = undefined