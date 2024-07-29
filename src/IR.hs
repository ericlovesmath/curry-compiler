{-# LANGUAGE TemplateHaskell #-}

module IR (makeIR, IR (..)) where

import qualified AST as A
import Control.Lens
import Control.Monad (liftM2, liftM3, liftM4, when)
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
    = BinaryOp A.BinOp IR IR
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

makeIR :: A.Ast -> IR
makeIR ast = evalState (ir ast) (FreshState 0 [Map.empty] 8)

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
        m : ms -> do
            when (Map.member name m) $
                error ("`let` called on already defined: " ++ name)
            scopes .= Map.insert name d m : ms
            maxDepth += 8
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
ir (A.BinaryOp f e e') = liftM2 (BinaryOp f) (ir e) (ir e')
ir (A.Int n) = return (Int n)
ir (A.Bool b) = return (Bool b)
ir (A.PrintInt e) = PrintInt <$> ir e
ir (A.If cond t f) = liftM4 If fresh (ir cond) (scoped (ir t)) (scoped (ir f))
ir (A.Begin es) = Begin <$> scoped (mapM ir es)
ir (A.While cond es) = liftM3 While fresh (ir cond) (scoped (mapM ir es))
ir (A.Var name) = Var <$> (getVar name)
ir (A.Set name e) = liftM2 Set (getVar name) (ir e)
ir (A.Let name e) = liftM2 Let (defineVar name) (scoped (ir e))
ir (A.Lambda arg body) = scoped $ liftM3 Lambda fresh (defineVar arg) (ir body)
ir (A.Apply fun arg) = liftM2 Apply (ir fun) (ir arg)
