module AST (makeAST, Ast (..), BinOp (..)) where

import qualified Parser as P

type Error = String

data Ast
    = BinaryOp BinOp Ast Ast
    | Int Integer
    | PrintInt Ast
    | Begin [Ast]
    | If Ast Ast Ast

data BinOp = Add | Sub | Mul | Div | Eq

makeAST :: P.Expr -> Either Error Ast
makeAST = tree

tree :: P.Expr -> Either Error Ast
tree (P.List [(P.Atom "+"), e, e']) = binaryOp Add e e'
tree (P.List [(P.Atom "-"), e, e']) = binaryOp Sub e e'
tree (P.List [(P.Atom "*"), e, e']) = binaryOp Mul e e'
tree (P.List [(P.Atom "/"), e, e']) = binaryOp Div e e'
tree (P.List [(P.Atom "="), e, e']) = binaryOp Eq e e'
tree (P.List [P.Atom "printint", e]) = PrintInt <$> tree e
tree (P.List (P.Atom "begin" : es)) = Begin <$> mapM tree es
tree (P.List [P.Atom "if", cond, t, f]) = If <$> tree cond <*> tree t <*> tree f
tree (P.Int n) = Right $ Int n
tree _ = Left "AST Failed"

binaryOp :: BinOp -> P.Expr -> P.Expr -> Either Error Ast
binaryOp binop expr expr' = do
    ast <- tree expr
    ast' <- tree expr'
    return $ BinaryOp binop ast ast'
