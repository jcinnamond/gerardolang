module Interpreter (eval, run, Value (..), Result (..)) where

import Data.Map.Lazy qualified as M
import Data.Text qualified as T
import Data.Time (LocalTime, defaultTimeLocale, formatTime)
import Data.Time.Format (parseTimeM)
import GHC.Show qualified (show)
import Parser (Expr (..), parse)

type FunctionSignature = [Text] -> Value -> EvalResult

data Function = Function
  { functionInputPyte :: Pyte
  , functionResultPyte :: Pyte
  , functionDefinition :: FunctionSignature
  }

functions :: Map Text Function
functions =
  M.fromList
    [ ("uuid", Function VoidPyte TextPyte evalUUID)
    , ("strip", Function TextPyte TextPyte evalStrip)
    , ("parseTime", Function VoidPyte TimePyte evalParseTime)
    , ("formatTime", Function TimePyte TextPyte evalFormatTime)
    ]

evalUUID :: FunctionSignature
evalUUID _ _ = pure $ Ok $ TextValue "123-456"

evalStrip :: FunctionSignature
evalStrip [x] (TextValue v) = pure $ Ok $ (TextValue . T.replace x "") v
evalStrip args _ = Left $ ArgumentError "evalStrip" 1 (length args)

evalParseTime :: FunctionSignature
evalParseTime [x] _ = pure $
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%s%Ez" (T.unpack x) of
    Nothing -> Err "error when calling `parseTime`: invalid time format"
    Just t -> Ok $ TimeValue t
evalParseTime args _ = Left $ ArgumentError "parseTime" 1 (length args)

evalFormatTime :: FunctionSignature
evalFormatTime [x] (TimeValue v) = pure $ Ok $ TextValue $ T.pack $ formatTime defaultTimeLocale (T.unpack x) v
evalFormatTime args _ = Left $ ArgumentError "formatTime" 1 (length args)

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

data Result
  = Ok Value
  | Err Text
  deriving stock (Show, Eq)

type EvalResult = Either EvalError Result

data EvalError
  = UnrecognisedCommand Text
  | IncorrectPyte Text Pyte Pyte
  | ArgumentError Text Int Int
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
  show (ArgumentError c e a) =
    "wrong number of arguments when calling "
      <> quote c
      <> ": expected "
      <> show e
      <> " but got "
      <> show a

type PyteCheckResult = Either EvalError Pyte

quote :: Text -> String
quote x = "`" <> T.unpack x <> "`"

run :: Text -> Either Text Result
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
pyteCheckBuiltin name p = case M.lookup name functions of
  Just (Function{functionInputPyte, functionResultPyte}) ->
    if functionInputPyte == p
      then pure functionResultPyte
      else Left $ IncorrectPyte name functionInputPyte p
  Nothing -> Left $ UnrecognisedCommand name

eval :: Expr -> EvalResult
eval e = go e (Ok Void)
 where
  go :: Expr -> Result -> EvalResult
  go (Call fn args) (Ok x) = callBuiltin fn args x
  go (Call _ _) err@(Err _) = pure err
  go (Pipe c exprs) x = case go c x of
    Left err -> Left err
    Right v -> go exprs v

callBuiltin :: Text -> [Text] -> Value -> EvalResult
callBuiltin name args v = case M.lookup name functions of
  Just (Function{functionDefinition}) -> functionDefinition args v
  Nothing -> Left $ UnrecognisedCommand name
