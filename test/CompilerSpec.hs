module CompilerSpec (spec) where

import System.Directory (removeFile)
import System.Process (callCommand, readProcess)
import Test.Hspec

import ASM (makeASM)
import AST (makeAST)
import IR (makeIR, IR)
import Parser (parse)

compile :: String -> IR
compile program =
    case parse program of
        Left err -> error $ "Parser Error: " ++ err
        Right expr -> case makeAST expr of
            Left err -> error $ "AST Error: " ++ err
            Right ast -> makeIR ast

verifyCompiler :: FilePath -> String -> IO ()
verifyCompiler filename expected = do
    let base = "test/tests/" ++ filename
    let inputFile = base ++ ".curry"
    let asmFile = base ++ ".asm"
    let binaryFile = base

    -- Read File
    code <- readFile inputFile

    -- Compile Code
    let ir = compile code
    makeASM ir asmFile

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
            verifyCompiler "basic" $
                unlines ["1", "9", "15", "-36", "-4", "-59", "2", "3", "2", "3"]
