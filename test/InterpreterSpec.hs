module InterpreterSpec (spec) where

import Interpreter (eval)
import Parser (Expr (..))
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
    it "interprets a single call" $
        eval (Call "uuid" []) `shouldBe` "123-456"

    it "interprets a pipe" $
        eval (Pipe (Call "uuid" []) (Call "strip" ["-"])) `shouldBe` "123456"
