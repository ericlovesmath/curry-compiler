module IR (makeIR, IR (..), BinOp (..)) where

import qualified AST as A

import Control.Monad.State

type Label = Integer
type FreshM = State Label

data IR
    = BinaryOp BinOp IR IR
    | Int Integer
    | PrintInt IR
    | Begin [IR]
    | If Label IR IR IR

data BinOp = Add | Sub | Mul | Div

makeIR :: A.Ast -> IR
makeIR ast = evalState (ir ast) 0

fresh :: FreshM Integer
fresh = do
    n <- get
    put (n + 1)
    return n

ir :: A.Ast -> FreshM IR
ir (A.BinaryOp op e e') = binaryOp op e e'
ir (A.Int n) = return $ Int n
ir (A.PrintInt e) = PrintInt <$> ir e
ir (A.Begin es) = Begin <$> mapM ir es
ir (A.If cond t f) = If <$> fresh <*> ir cond <*> ir t <*> ir f

binaryOp :: A.BinOp -> A.Ast -> A.Ast -> FreshM IR
binaryOp op expr expr' = do
    e <- ir expr
    e' <- ir expr'
    return $ BinaryOp (to op) e e'
  where
    to A.Add = Add
    to A.Sub = Sub
    to A.Mul = Mul
    to A.Div = Div
