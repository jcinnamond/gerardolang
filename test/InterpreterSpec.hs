module InterpreterSpec (spec) where

import Data.Time (LocalTime, defaultTimeLocale, parseTimeM)
import Interpreter (Value (..), eval)
import Parser (Expr (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    it "interprets a single call" $
        eval (Call "uuid" []) `shouldBe` "123-456"

    it "interprets a pipe" $
        eval (Pipe (Call "uuid" []) (Call "strip" ["-"])) `shouldBe` "123456"

    describe "commands" $ do
        it "handles parseTime" $ do
            let Just expectedTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%s%Ez" "2023-04-06T21:14:00+01:00" :: Maybe LocalTime
            eval (Call "parseTime" ["2023-04-06T21:14:00+01:00"]) `shouldBe` TimeValue expectedTime
