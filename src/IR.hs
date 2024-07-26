{-# LANGUAGE TemplateHaskell #-}

module IR (makeIR, IR (..), BinOp (..)) where

import qualified AST as A
import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

type Label = Integer
type Depth = Integer
type VarMap = Map String Depth

data FreshState = FreshState
    { _label :: Label
    , _scopes :: [VarMap]
    , _maxDepth :: Depth
    }
makeLenses ''FreshState

type FreshM = State FreshState

data IR
    = BinaryOp BinOp IR IR
    | Int Integer
    | Bool Bool
    | PrintInt IR
    | Begin [IR]
    | Var Depth
    | Set Depth IR
    | Let Depth IR
    | While Label IR [IR]
    | If Label IR IR IR
    | Lambda Label Depth IR
    | Apply IR IR

data BinOp = Add | Sub | Mul | Div | Eq

makeIR :: A.Ast -> IR
makeIR ast = evalState (ir ast) (FreshState 0 [Map.empty] 1)

fresh :: FreshM Integer
fresh = do
    label += 1
    n <- use label
    return n

-- Fetches variable, fails if not defined in any higher scope
getVar :: String -> FreshM Depth
getVar name = do
    scopes' <- use scopes
    let search [] = error $ "failed to find variable: " ++ name
        search (m : ms) = case Map.lookup name m of
            Nothing -> search ms
            Just d -> d
    return $ search scopes'

-- Defines variable, fails with already defined in scope
defineVar :: String -> FreshM Depth
defineVar name = do
    scopes' <- use scopes
    d <- use maxDepth
    case scopes' of
        [] -> error "IR unexpectedly escaped global scope"
        m : ms ->
            if Map.member name m
                then error $ "`let` called on already defined: " ++ name
                else do
                    scopes .= Map.insert name d m : ms
                    maxDepth += 1
                    return d

scoped :: FreshM a -> FreshM a
scoped action = do
    scopes %= (Map.empty :)
    result <- action
    scopes %= unsafeTail
    return result
  where
    unsafeTail [] = error "IR unexpectedly escaped global scope"
    unsafeTail (_ : es) = es

ir :: A.Ast -> FreshM IR
ir (A.BinaryOp binop e e') = BinaryOp (ir_op binop) <$> ir e <*> ir e'
  where
    ir_op A.Add = Add
    ir_op A.Sub = Sub
    ir_op A.Mul = Mul
    ir_op A.Div = Div
    ir_op A.Eq = Eq
ir (A.Int n) = return $ Int n
ir (A.Bool b) = return $ Bool b
ir (A.PrintInt e) = PrintInt <$> ir e
ir (A.If cond t f) = If <$> fresh <*> ir cond <*> scoped (ir t) <*> scoped (ir f)
ir (A.Begin es) = scoped $ Begin <$> mapM ir es
ir (A.While cond es) = While <$> fresh <*> ir cond <*> scoped (mapM ir es)
ir (A.Var name) = getVar name >>= (\d -> return $ Var d)
ir (A.Set name e) = getVar name >>= (\d -> Set d <$> ir e)
ir (A.Let name e) = defineVar name >>= (\d -> Let d <$> scoped (ir e))
ir (A.Lambda arg body) = scoped $ Lambda <$> fresh <*> defineVar arg <*> ir body
ir (A.Apply fun arg) = Apply <$> ir fun <*> ir arg
