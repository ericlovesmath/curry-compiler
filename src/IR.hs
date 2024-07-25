module IR (makeIR, IR (..), BinOp (..)) where

import qualified AST as A
import Data.Map as Map

import Control.Monad.State

type Label = Integer
type Depth = Integer
type VarMap = Map String Depth
type FreshM = State (Label, [VarMap], Depth)

data IR
    = BinaryOp BinOp IR IR
    | Int Integer
    | Bool Bool
    | PrintInt IR
    | Begin [IR]
    | Var Depth
    | Set Depth IR
    | Define Depth IR
    | If Label IR IR IR

data BinOp = Add | Sub | Mul | Div | Eq

makeIR :: A.Ast -> IR
makeIR ast = evalState (ir ast) (0, [Map.empty], 1)

fresh :: FreshM Integer
fresh = do
    (n, scopes, d) <- get
    put (n + 1, scopes, d)
    return n

-- TODO: Inline all these and move fail to Error

-- Fetches variable, fails if not defined in any higher scope
getVar :: String -> FreshM Depth
getVar name = do
    (_, scope, _) <- get
    let search [] = error $ "failed to find variable: " ++ name
        search (m : ms) = case Map.lookup name m of
            Nothing -> search ms
            Just d -> d
    return $ search scope

-- Defines variable, fails with already defined in scope
defineVar :: String -> FreshM Depth
defineVar name = do
    (n, scope, d) <- get
    let m : ms = scope
    if Map.member name m
        then error $ "`define` called on already defined variable: " ++ name
        else do
            put (n, Map.insert name d m : ms, d + 1)
            return d

enterScope :: FreshM ()
enterScope = do
    (n, scope, d) <- get
    put (n, Map.empty : scope, d)

leaveScope :: FreshM ()
leaveScope = do
    (n, scope, d) <- get
    put (n, tail scope, d)

ir :: A.Ast -> FreshM IR
ir (A.BinaryOp op e e') = BinaryOp (to op) <$> ir e <*> ir e'
  where
    to A.Add = Add
    to A.Sub = Sub
    to A.Mul = Mul
    to A.Div = Div
    to A.Eq = Eq
ir (A.Int n) = return $ Int n
ir (A.Bool b) = return $ Bool b
ir (A.PrintInt e) = PrintInt <$> ir e
ir (A.If cond t f) = If <$> fresh <*> ir cond <*> ir t <*> ir f
ir (A.Begin es) = do
    enterScope
    result <- mapM ir es
    leaveScope
    return $ Begin result
ir (A.Var name) = do
    depth <- getVar name
    return $ Var depth
ir (A.Set name e) = do
    depth <- getVar name
    Set depth <$> ir e
ir (A.Define name e) = do
    depth <- defineVar name
    Define depth <$> ir e
