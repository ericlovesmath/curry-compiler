module IR (makeIR, IR (..)) where

import qualified AST as A

import Control.Monad.State

type Label = Integer
type FreshM = State Label

data IR
    = Add IR IR
    | Sub IR IR
    | Mul IR IR
    | Div IR IR
    | Int Integer
    | PrintInt IR
    | Begin [IR]
    | If Label IR IR IR

makeIR :: A.Ast -> IR
makeIR ast = evalState (ir ast) 0

fresh :: FreshM Integer
fresh = do
    n <- get
    put (n + 1)
    return n

ir :: A.Ast -> FreshM IR
ir (A.Add e e') = binaryOp Add e e'
ir (A.Sub e e') = binaryOp Sub e e'
ir (A.Mul e e') = binaryOp Mul e e'
ir (A.Div e e') = binaryOp Div e e'
ir (A.Int n) = return $ Int n
ir (A.PrintInt e) = PrintInt <$> ir e
ir (A.Begin es) = Begin <$> mapM ir es
ir (A.If cond t f) = If <$> fresh <*> ir cond <*> ir t <*> ir f

binaryOp :: (IR -> IR -> IR) -> A.Ast -> A.Ast -> FreshM IR
binaryOp constructor expr expr' = do
    e <- ir expr
    e' <- ir expr'
    return $ constructor e e'
