module InterpreterSpec (spec) where

import Data.Time (LocalTime (..), TimeOfDay (TimeOfDay))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Interpreter (Value (..), eval, run)
import Parser (Expr (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    it "interprets a single call" $
        eval (Call "uuid" []) `shouldBe` Right "123-456"

    it "interprets a pipe" $
        eval (Pipe (Call "uuid" []) (Call "strip" ["-"])) `shouldBe` Right "123456"

    describe "commands" $ do
        it "handles parseTime" $ do
            let expectedTime = LocalTime (fromOrdinalDate 2023 103) (TimeOfDay 21 32 0)
            eval (Call "parseTime" ["2023-04-13T21:32:00+01:00"]) `shouldBe` Right (TimeValue expectedTime)

    describe "run" $ do
        it "errors when calling uuid with an argument" $ do
            run "uuid |> uuid" `shouldBe` Left "function `uuid` does not take any arguments"

        it "errors when passing the wrong type" $ do
            run "parseTime \"2023-04-13T21:32:00+01:00\" |> strip \"-\"" `shouldBe` Left "unexpected argument when calling `strip`: expected <Text> but got <Time>"

        it "errors when calling a function that does not exist" $ do
            run "rubbish" `shouldBe` Left "unrecognised function `rubbish`"
