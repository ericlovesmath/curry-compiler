module Main where

import Parser (parse)
import AST (toAst)

main :: IO ()
main = print . fmap toAst $ parse "(- 23 (+ 123 23 ))"

-- TODO: Write tests
