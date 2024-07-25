module IR (makeIR, IR (..), BinOp (..)) where

import qualified AST as A
import Data.Map as Map

import Control.Monad.State

type Label = Integer
type Depth = Integer
type VarMap = Map String Depth
type Scope = [(VarMap, Depth)]
type FreshM = State (Label, Scope)

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
makeIR ast = evalState (ir ast) (0, [(Map.empty, 1)])

fresh :: FreshM Integer
fresh = do
    (n, scopes) <- get
    put (n + 1, scopes)
    return n

getDepth :: String -> FreshM (Maybe Depth)
getDepth name = do
    (_, scope) <- get
    let search [] = Nothing
        search ((m, _) : ms) = maybe (search ms) Just (Map.lookup name m)
    return $ search scope

setDepth :: String -> Depth -> FreshM ()
setDepth name depth = do
    (n, scope) <- get
    let (m, d) : ms = scope
    put (n, (Map.insert name depth m, d) : ms)

freshVar :: FreshM Depth
freshVar = do
    (n, scope) <- get
    let (m, d) : ms = scope
    let newDepth = d + 1
    put (n, (m, newDepth) : ms)
    return d

enterScope :: FreshM ()
enterScope = do
    (n, scope) <- get
    put (n, (Map.empty, 1) : scope)

leaveScope :: FreshM ()
leaveScope = do
    (n, scope) <- get
    put (n, tail scope)

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
ir (A.Begin es) = Begin <$> mapM ir es
-- TODO: if Begin was a scope, this is how we would do it
-- ir (A.Begin es) = do
--     enterScope
--     result <- mapM ir es
--     leaveScope
--     return $ Begin result
ir (A.Var name) = do
    depth <- getDepth name
    case depth of
        Just d -> return $ Var d
        Nothing -> error $ "Var called before Set: " ++ name
ir (A.Set name e) = do
    depth <- getDepth name
    case depth of
        Just d -> Set d <$> ir e
        Nothing -> do
            d <- freshVar
            setDepth name d
            Set d <$> ir e
ir (A.Define name e) = do
    do
        d <- freshVar
        setDepth name d
        Define d <$> ir e
