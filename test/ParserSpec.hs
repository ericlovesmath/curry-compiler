module ParserSpec (spec) where

import Parser (Expr (..), parse)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

fails :: Either a b -> Bool
fails (Left _) = True
fails (Right _) = False

newtype ValidExpr = ValidExpr Expr deriving (Eq, Show)

instance Arbitrary ValidExpr where
    arbitrary = ValidExpr <$> arbitraryExpr 4

arbitraryExpr :: Integer -> Gen Expr
arbitraryExpr depth = case depth of
    0 -> oneof cands
    n -> oneof $ (List <$> arbitraryList (n - 1)) : cands
  where
    cands =
        [ Atom <$> arbitraryAtom
        , Atom . pure <$> arbitrarySpecial
        , Int <$> arbitrary
        , Bool <$> arbitrary
        ]
    arbitraryAtom = listOf1 $ elements ['a' .. 'z']
    arbitrarySpecial = elements "+-/*"
    arbitraryList d = listOf $ arbitraryExpr (d - 1)

spec :: Spec
spec = do
    describe "Parser pass" $ do
        it "basic 1" $ do
            parse "1" `shouldBe` Right (Int 1)
        it "basic 2" $ do
            parse "0" `shouldBe` Right (Int 0)
        it "empty list" $ do
            parse "()" `shouldBe` Right (List [])
        it "generic" $ do
            parse "(+ 1 2)" `shouldBe` Right (List [Atom "+", Int 1, Int 2])
        it "special chars" $ do
            parse "(+ - / * #t #f)" `shouldBe` Right (List $ (Atom . pure <$> "+-/*") ++ (Bool <$> [True, False]))
        it "spaces" $ do
            parse "(  word   12   5    6  )"
                `shouldBe` Right (List [Atom "word", Int 12, Int 5, Int 6])
        it "other whitespace" $ do
            parse "( \r word \n       12   5    6  )"
                `shouldBe` Right (List [Atom "word", Int 12, Int 5, Int 6])
        it "nested" $ do
            parse "(1 (2 (3 4 5)) (6 7) 2)"
                `shouldBe` Right (List [Int 1, List [Int 2, List [Int 3, Int 4, Int 5]], List [Int 6, Int 7], Int 2])
        it "empty empty list" $ do
            parse "(() (() ()))" `shouldBe` Right (List [List [], List [List [], List []]])

    describe "Parser fail" $ do
        it "unbalanced parens" $ do
            parse "(1 2 " `shouldSatisfy` fails
            parse "1 2)" `shouldSatisfy` fails
            parse "(1 (33 2)" `shouldSatisfy` fails
        it "leading and training spaces" $ do
            parse "  (1 2)" `shouldSatisfy` fails
            parse "(1 2)  " `shouldSatisfy` fails
        it "empty input" $ do
            parse "" `shouldSatisfy` fails
        it "no repeat bool" $ do
            parse "(#t#t)" `shouldSatisfy` fails
            parse "(#f#f)" `shouldSatisfy` fails
        it "no repeat special chars" $ do
            parse "(++ --)" `shouldSatisfy` fails

    describe "Parser pass quickcheck" $ do
        modifyMaxSuccess (const 10000) $
            prop "random valid exprs" $
                \(ValidExpr expr) -> (parse . show $ expr) `shouldBe` (Right expr)
