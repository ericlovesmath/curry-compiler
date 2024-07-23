module AST (tree, Ast (..)) where

import qualified Parser as P

type Error = String

data Ast
    = Add Ast Ast
    | Sub Ast Ast
    | Mul Ast Ast
    | Div Ast Ast
    | Int Integer
    | PrintInt Ast
    deriving (Show)

tree :: P.Expr -> Either Error Ast
tree (P.List [(P.Atom "+"), e, e']) = binaryOp Add e e'
tree (P.List [(P.Atom "-"), e, e']) = binaryOp Sub e e'
tree (P.List [(P.Atom "*"), e, e']) = binaryOp Mul e e'
tree (P.List [(P.Atom "/"), e, e']) = binaryOp Div e e'
tree (P.List [P.Atom "printint", e]) = PrintInt <$> tree e
tree (P.Int n) = Right $ Int n
tree _ = Left "invalid"

binaryOp :: (Ast -> Ast -> Ast) -> P.Expr -> P.Expr -> Either Error Ast
binaryOp constructor expr expr' = do
    ast <- tree expr
    ast' <- tree expr'
    return $ constructor ast ast'
