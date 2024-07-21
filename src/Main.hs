module Main where

import ASM (compile)
import AST (toAst)
import Parser (parse)

main :: IO ()
main = case parse "(- 79 (+ 123 23 ))" >>= toAst of
    Right ast -> writeFile "output.asm" $ compile ast
    Left err -> putStr err
