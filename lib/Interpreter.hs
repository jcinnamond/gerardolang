module Interpreter (eval, run, Value (..)) where

import Data.Text as T
import Data.Time (LocalTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import GHC.Show qualified (show)
import Parser (Expr (..), parse)

data Value
    = TextValue Text
    | TimeValue LocalTime
    | Void
    deriving stock (Show, Eq)

instance IsString Value where
    fromString = TextValue . T.pack

type EvalResult = Either EvalError Value

data EvalError
    = UnrecognisedCommand Text
    | UnexpectedArg Text
    | IncorrectArgType Text Text Text
    deriving stock (Eq)

instance Show EvalError where
    show (UnrecognisedCommand c) = "unrecognised function " <> quote c
    show (UnexpectedArg c) = "function " <> quote c <> " does not take any arguments"
    show (IncorrectArgType c e a) =
        "unexpected argument when calling "
            <> quote c
            <> ": expected "
            <> T.unpack e
            <> " but got "
            <> T.unpack a

quote :: Text -> String
quote x = "`" <> T.unpack x <> "`"

run :: Text -> Either Text Value
run inp = case parse inp of
    Left err -> Left $ show err
    Right expr -> case eval expr of
        Left err -> Left $ show err
        Right v -> Right v

eval :: Expr -> EvalResult
eval e = go e Void
  where
    go :: Expr -> Value -> EvalResult
    go (Call fn args) x = callBuiltin fn args x
    go (Pipe c exprs) x = case go c x of
        Left err -> Left err
        Right v -> go exprs v

callBuiltin :: Text -> [Text] -> Value -> EvalResult
callBuiltin "uuid" [] v = noArgs "uuid" v $ TextValue "123-456"
callBuiltin "strip" [x] v = textArg "strip" v (TextValue . T.replace x "")
callBuiltin "parseTime" [x] v = noArgs "parseTime" v $
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%s%Ez" (T.unpack x) of
        Nothing -> error "parseTime error: bad time format"
        Just t -> TimeValue t
callBuiltin fn _ _ = Left $ UnrecognisedCommand fn

noArgs :: Text -> Value -> Value -> EvalResult
noArgs _ Void v = pure v
noArgs n _ _ = Left $ UnexpectedArg n

textArg :: Text -> Value -> (Text -> Value) -> EvalResult
textArg _ (TextValue v) fn = pure $ fn v
textArg n v _ = Left $ IncorrectArgType n "<Text>" (typeOf v)

typeOf :: Value -> Text
typeOf (TextValue _) = "<Text>"
typeOf (TimeValue _) = "<Time>"
typeOf Void = "Void"