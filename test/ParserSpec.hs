module ParserSpec (spec) where

import Parser (Expr (..), parse)
import Test.Hspec (Spec, it)
import Test.Hspec.Megaparsec (shouldParse)

spec :: Spec
spec = do
    it "parses a call" $
        parse "uuid" `shouldParse` Call "uuid" []

    it "parses a pipe" $
        parse "uuid |> strip" `shouldParse` Pipe (Call "uuid" []) (Call "strip" [])

    it "parses a pipe and an argument" $
        parse "uuid \"thing\" |> strip" `shouldParse` Pipe (Call "uuid" ["thing"]) (Call "strip" [])

    it "parses pipelines" $
        parse "uuid |> strip \"-\" |> lower"
            `shouldParse` Pipe (Call "uuid" []) (Pipe (Call "strip" ["-"]) (Call "lower" []))

    it "handles newlines" $
        parse "uuid |> strip \"-\" \n"
            `shouldParse` Pipe (Call "uuid" []) (Call "strip" ["-"])
