module Main where

import System.IO (hFlush, stdout)
import System.Process (callCommand, readProcess)

import ASM (makeASM)
import AST (makeAST)
import IR (makeIR)
import Parser (parse)

type Error = String

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
        ":q" -> return ()
        _ -> do
            case parse input >>= makeAST of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                Right ast -> do
                    let ir = makeIR ast
                    makeASM ir $ path ++ ".asm"
                    callCommand $ "make -s FNAME=" ++ path
                    output <- readProcess path [] ""
                    putStr output
            repl
  where
    path = "/tmp/curry-repl"

main :: IO ()
main = repl
