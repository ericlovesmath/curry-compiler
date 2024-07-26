module AST (makeAST, Ast (..), BinOp (..)) where

import qualified Parser as P

type Error = String

data Ast
    = BinaryOp BinOp Ast Ast
    | Int Integer
    | Bool Bool
    | PrintInt Ast
    | Begin [Ast]
    | Var String
    | Set String Ast
    | Let String Ast
    | While Ast [Ast]
    | If Ast Ast Ast
    | Lambda String Ast
    | Apply Ast Ast

data BinOp = Add | Sub | Mul | Div | Eq

makeAST :: P.Expr -> Either Error Ast
makeAST = tree

tree :: P.Expr -> Either Error Ast
tree (P.List [(P.Atom "+"), e, e']) = binaryOp Add e e'
tree (P.List [(P.Atom "-"), e, e']) = binaryOp Sub e e'
tree (P.List [(P.Atom "*"), e, e']) = binaryOp Mul e e'
tree (P.List [(P.Atom "/"), e, e']) = binaryOp Div e e'
tree (P.List [(P.Atom "="), e, e']) = binaryOp Eq e e'
tree (P.List [P.Atom "put", e]) = PrintInt <$> tree e
tree (P.List (P.Atom "begin" : es)) = Begin <$> mapM tree es
tree (P.List [P.Atom "if", cond, t, f]) = If <$> tree cond <*> tree t <*> tree f
tree (P.List [P.Atom "set", P.Atom name, val]) = Set name <$> tree val
tree (P.List [P.Atom "let", P.Atom name, val]) = Let name <$> tree val
tree (P.List [P.Atom "define", P.Atom name, args, e]) =
    Let name <$> tree (P.List [P.Atom "lambda", args, e])
tree (P.List (P.Atom "while" : cond : es@(_ : _))) = While <$> tree cond <*> mapM tree es
tree (P.List [P.Atom "lambda", P.Atom arg, e]) = Lambda arg <$> tree e
tree (P.List [P.Atom "lambda", P.List args@(_ : _), e]) = go args
  where
    go [] = tree e
    go (P.Atom v : vs) = Lambda v <$> go vs
    go _ = Left "`lambda` attempted to be defined with non-variable args"
tree (P.List [f, arg]) = Apply <$> tree f <*> tree arg
tree (P.List (f : args@(_ : _))) = go $ reverse args
  where
    go [] = tree f
    go (v : vs) = Apply <$> go vs <*> tree v
tree (P.Int n) = Right $ Int n
tree (P.Bool b) = Right $ Bool b
tree (P.List [e]) = tree e
tree (P.Atom name) = Right $ Var name
tree _ = Left "AST Failed"

binaryOp :: BinOp -> P.Expr -> P.Expr -> Either Error Ast
binaryOp binop expr expr' = do
    ast <- tree expr
    ast' <- tree expr'
    return $ BinaryOp binop ast ast'
