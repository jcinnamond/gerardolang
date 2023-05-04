module InterpreterSpec (spec) where

import Data.Time (LocalTime (..), TimeOfDay (TimeOfDay))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Interpreter (Result (..), Value (..), eval, run)
import Parser (Expr (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "commands" $ do
        it "handles uuid" $ do
            eval (Call "uuid" []) `shouldBe` Right (Ok "123-456")

        it "handles parseTime" $ do
            let expectedTime = LocalTime (fromOrdinalDate 2023 103) (TimeOfDay 21 32 0)
            eval (Call "parseTime" ["2023-04-13T21:32:00+01:00"]) `shouldBe` Right (Ok $ TimeValue expectedTime)

        it "handles formatTime" $ do
            run "parseTime \"2023-04-13T21:32:00+01:00\" |> formatTime \"%Y-%m-%d\"" `shouldBe` Right (Ok "2023-04-13")

    describe "run" $ do
        it "errors when calling uuid with an argument" $ do
            run "uuid |> uuid" `shouldBe` Left "function `uuid` does not take any arguments"

        it "errors when passing the wrong type" $ do
            run "parseTime \"2023-04-13T21:32:00+01:00\" |> strip \"-\"" `shouldBe` Left "unexpected argument type when calling `strip`: expected <Text> but got <Time>"

        it "errors when calling a function that does not exist" $ do
            run "rubbish" `shouldBe` Left "unrecognised function `rubbish`"

        it "interprets a pipe" $
            run "uuid |> strip \"-\"" `shouldBe` Right (Ok "123456")

    describe "errors" $ do
        it "can be returned from functions" $ do
            run "parseTime \"nonsense\"" `shouldBe` Right (Err "error when calling `parseTime`: invalid time format")

        it "stops processing the pipeline" $ do
            run "parseTime \"nonsense\" |> formatTime \"%Y-%m-%d\"" `shouldBe` Right (Err "error when calling `parseTime`: invalid time format")
