module AST (tree, Ast (..)) where

import qualified Parser as P

type Error = String

data Ast = Add Ast Ast | Sub Ast Ast | Mul Ast Ast | Div Ast Ast | Int Integer deriving (Show)

tree :: P.Expr -> Either Error Ast
tree (P.List [(P.Atom "+"), expr, expr']) = binaryOp Add expr expr'
tree (P.List [(P.Atom "-"), expr, expr']) = binaryOp Sub expr expr'
tree (P.List [(P.Atom "*"), expr, expr']) = binaryOp Mul expr expr'
tree (P.List [(P.Atom "/"), expr, expr']) = binaryOp Div expr expr'
tree (P.Int n) = Right $ Int n
tree _ = Left "TODO"

binaryOp :: (Ast -> Ast -> Ast) -> P.Expr -> P.Expr -> Either Error Ast
binaryOp constructor expr expr' = do
    ast <- tree expr
    ast' <- tree expr'
    return $ constructor ast ast'
