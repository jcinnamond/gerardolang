module Interpreter (eval, run, Value (..)) where

import Data.Text as T
import Data.Time (LocalTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import GHC.Show qualified (show)
import Parser (Expr (..), parse)

data Pyte
  = TextPyte
  | TimePyte
  | VoidPyte
  deriving stock (Eq)

instance Show Pyte where
  show TextPyte = "<Text>"
  show TimePyte = "<Time>"
  show VoidPyte = "<Void>"

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
  | IncorrectPyte Text Pyte Pyte
  deriving stock (Eq)

instance Show EvalError where
  show (UnrecognisedCommand c) = "unrecognised function " <> quote c
  show (IncorrectPyte c VoidPyte _) = "function " <> quote c <> " does not take any arguments"
  show (IncorrectPyte c e a) =
    "unexpected argument type when calling "
      <> quote c
      <> ": expected "
      <> show e
      <> " but got "
      <> show a

type PyteCheckResult = Either EvalError Pyte

quote :: Text -> String
quote x = "`" <> T.unpack x <> "`"

run :: Text -> Either Text Value
run inp = do
  expr <- parse inp
  case pyteCheck expr of
    Left err -> Left $ show err
    Right _ -> case eval expr of
      Left err -> Left $ show err
      Right v -> Right v

pyteCheck :: Expr -> PyteCheckResult
pyteCheck e = go e VoidPyte
 where
  go :: Expr -> Pyte -> PyteCheckResult
  go (Call fn _) p = pyteCheckBuiltin fn p
  go (Pipe c exprs) p = case go c p of
    Left err -> Left err
    Right p' -> go exprs p'

pyteCheckBuiltin :: Text -> Pyte -> PyteCheckResult
pyteCheckBuiltin "uuid" p = expectVoid "uuid" p TextPyte
pyteCheckBuiltin "strip" p = expectText "strip" p TextPyte
pyteCheckBuiltin "parseTime" p = expectVoid "parseTime" p TimePyte
pyteCheckBuiltin c _ = Left $ UnrecognisedCommand c

expectVoid :: Text -> Pyte -> Pyte -> PyteCheckResult
expectVoid _ VoidPyte p = pure p
expectVoid c p _ = Left $ IncorrectPyte c VoidPyte p

expectText :: Text -> Pyte -> Pyte -> PyteCheckResult
expectText _ TextPyte p = pure p
expectText c p _ = Left $ IncorrectPyte c TextPyte p

eval :: Expr -> EvalResult
eval e = go e Void
 where
  go :: Expr -> Value -> EvalResult
  go (Call fn args) x = callBuiltin fn args x
  go (Pipe c exprs) x = case go c x of
    Left err -> Left err
    Right v -> go exprs v

callBuiltin :: Text -> [Text] -> Value -> EvalResult
callBuiltin "uuid" [] _ = pure $ TextValue "123-456"
callBuiltin "strip" [x] (TextValue v) = pure $ (TextValue . T.replace x "") v
callBuiltin "parseTime" [x] _ = pure $
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%s%Ez" (T.unpack x) of
    Nothing -> error "parseTime error: bad time format"
    Just t -> TimeValue t
callBuiltin fn _ _ = Left $ UnrecognisedCommand fn