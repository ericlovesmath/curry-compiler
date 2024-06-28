module ParserSpec (spec) where

import Test.Hspec
import Parser (parse, Expr (..))

spec :: Spec
spec = do
  describe "Parser" $ do
    it " the first element of a list" $ do
      parse "1" `shouldBe` Right (Int 1)
