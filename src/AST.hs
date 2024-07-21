module AST (toAst, Ast (..)) where

import qualified Parser as P

type Error = String

data Ast = Add Ast Ast | Sub Ast Ast | Int Integer deriving (Show)

toAst :: P.Expr -> Either Error Ast
toAst (P.List [(P.Atom "+"), expr, expr']) = do
    ast <- toAst expr
    ast' <- toAst expr'
    return $ Add ast ast'
toAst (P.List [(P.Atom "-"), expr, expr']) = do
    ast <- toAst expr
    ast' <- toAst expr'
    return $ Sub ast ast'
toAst (P.Int n) = Right $ Int n
toAst _ = Left "TODO"
