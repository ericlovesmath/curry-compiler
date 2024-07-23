module IR (makeIR, IR (..)) where

import Control.Monad.State

import AST as A

type Label = Integer

data IR
    = Add IR IR
    | Sub IR IR
    | Mul IR IR
    | Div IR IR
    | Int Integer
    | PrintInt IR
    | Begin [IR]
    | If Label IR IR IR

type FreshM = State Label

fresh :: FreshM Integer
fresh = do
    n <- get
    put (n + 1)
    return n

binaryOp :: (IR -> IR -> IR) -> Ast -> Ast -> FreshM IR
binaryOp constructor expr expr' = do
    e <- ir expr
    e' <- ir expr'
    return $ constructor e e'

ir :: Ast -> FreshM IR
ir ast = case ast of
    A.Add e e' -> binaryOp IR.Add e e'
    A.Sub e e' -> binaryOp IR.Sub e e'
    A.Mul e e' -> binaryOp IR.Mul e e'
    A.Div e e' -> binaryOp IR.Div e e'
    A.Int n -> return $ IR.Int n
    A.PrintInt e -> IR.PrintInt <$> ir e
    A.Begin es -> IR.Begin <$> mapM ir es
    A.If cond t f -> IR.If <$> fresh <*> ir cond <*> ir t <*> ir f

makeIR :: Ast -> IR
makeIR ast = evalState (ir ast) 0
