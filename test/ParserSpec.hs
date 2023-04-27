module ParserSpec (spec) where

import Parser (Expr (..), parse)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
    it "parses a call" $
        parse "uuid" `shouldBe` Right (Call "uuid" [])

    it "parses a pipe" $
        parse "uuid |> strip" `shouldBe` Right (Pipe (Call "uuid" []) (Call "strip" []))

    it "parses a pipe and an argument" $
        parse "uuid \"thing\" |> strip" `shouldBe` Right (Pipe (Call "uuid" ["thing"]) (Call "strip" []))

    it "parses pipelines" $
        parse "uuid |> strip \"-\" |> lower"
            `shouldBe` Right (Pipe (Call "uuid" []) (Pipe (Call "strip" ["-"]) (Call "lower" [])))

    it "handles newlines" $
        parse "uuid |> strip \"-\" \n"
            `shouldBe` Right (Pipe (Call "uuid" []) (Call "strip" ["-"]))
