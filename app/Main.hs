module Main where

import Data.Text.IO (interact)
import Interpreter (eval)
import Parser (parse)

main :: IO ()
main = interact runPipeline

runPipeline :: Text -> Text
runPipeline s = case eval <$> parse s of
    Left err -> show err
    Right res -> show res
