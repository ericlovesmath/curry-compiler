module CompilerSpec (spec) where

import System.Directory (removeFile)
import System.Process (callCommand, readProcess)
import Test.Hspec

import ASM (asm)
import AST (tree)
import Parser (parse)

compile :: String -> String
compile program =
    case parse program of
        Left err -> error $ "Parser Error: " ++ err
        Right expr -> case tree expr of
            Left err -> error $ "AST Error: " ++ err
            Right ast -> asm ast

verifyCompiler :: FilePath -> String -> IO ()
verifyCompiler filename expected = do
    let base = "test/tests/" ++ filename
    let inputFile = base ++ ".curry"
    let asmFile = base ++ ".asm"
    let binaryFile = base

    -- Read File
    code <- readFile inputFile

    -- Compile Code
    writeFile asmFile (compile code)

    -- Assemble Binary
    callCommand $ "make -s FNAME=" ++ base

    -- Verify Binary
    output <- readProcess binaryFile [] ""
    output `shouldBe` expected

    -- Cleanup
    removeFile asmFile
    removeFile binaryFile

spec :: Spec
spec = do
    describe "compiler checks" $ do
        it "compiles source code to assembly and runs correctly" $ do
            verifyCompiler "basic" "-59\n"
