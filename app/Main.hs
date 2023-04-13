module Main where

import Data.Text.IO (interact)
import Interpreter (run)

main :: IO ()
main = interact runPipeline

runPipeline :: Text -> Text
runPipeline s = case run s of
    Left err -> "!!! Error: " <> show err
    Right res -> show res
