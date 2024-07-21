module Main where

import ASM (asm)
import AST (tree)
import Parser (parse)

compile :: String -> FilePath -> IO ()
compile program filepath =
    case parse program of
        Left err -> putStrLn $ "Parser Error: " ++ err
        Right expr -> case tree expr of
            Left err -> putStrLn $ "Compiler Error: " ++ err
            Right ast -> writeFile filepath $ asm ast

main :: IO ()
main = compile "(* (+ 2700 (/ -127396 51)) 2)" "output.asm"
