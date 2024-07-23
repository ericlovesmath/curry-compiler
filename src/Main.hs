module Main where

import System.IO (hFlush, stdout)
import System.Process (callCommand, readProcess)

import ASM (asm)
import IR (makeIR)
import AST (tree)
import Parser (parse)

type Error = String

compile :: String -> Either Error String
compile program = do
    expr <- parse program
    ast <- tree expr
    let ir = makeIR ast
    return $ asm ir

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
        ":q" -> return ()
        _ -> do
            case compile input of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                Right assembly -> do
                    writeFile (path ++ ".asm") assembly
                    callCommand $ "make -s FNAME=" ++ path
                    output <- readProcess path [] ""
                    putStr output
            repl
  where
    path = "/tmp/curry-repl"

main :: IO ()
main = repl
