module ASM (makeASM) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import IR

type Label = String
type Log = Map Label String
type Writer = State Log

makeASM :: IR -> FilePath -> IO ()
makeASM ir filepath = do
    let output [] = ""
        output (m : ms) = "\n" ++ fst m ++ ":\n" ++ snd m ++ "\n" ++ output ms
    let codeMap = execState emitter Map.empty
    let res = output (Map.toList codeMap)
    writeFile filepath (prologue ++ res)
  where
    emitter = do
        emit "_main" ir
        code "_main" epilogue
    prologue =
        unlines
            [ "global _main"
            , "extern _printf"
            , "extern _exit\n"
            , "section .data"
            , "default rel"
            , "format_int: db \"%d\", 10, 0"
            , "section .text"
            ]
    epilogue = unlines ["xor rdi, rdi", "call _exit"]

code :: Label -> String -> Writer ()
code label line = do
    m <- get
    put $ Map.insertWith (\old new -> new ++ '\n' : old) label line m

emit :: Label -> IR -> Writer ()
emit s (Int n) = code s $ "mov rax, " ++ show n
emit s (Bool True) = emit s $ Int 1
emit s (Bool False) = emit s $ Int 0
emit s (Begin es) = mapM_ (emit s) es
emit s (BinaryOp op left right) = do
    emit s right
    code s "push rax"
    emit s left
    code s "pop rdi"
    code s $ case op of
        Add -> "add rax, rdi"
        Sub -> "sub rax, rdi"
        Mul -> "imul rax, rdi"
        Div -> "cqo\n" ++ "idiv rdi"
        Eq -> "cmp rax, rdi\n" ++ "sete al\n" ++ "movzx rax, al"
emit s (PrintInt e) = do
    emit s e
    code s "lea rdi, [format_int]"
    code s "mov rsi, rax"
    code s "xor rax, rax"
    code s "call _printf"
emit s (If label cond true false) = do
    let l = show label
    emit s $ cond
    code s $ "cmp rax, 0"
    code s $ "je _else_" ++ l
    emit s $ true
    code s $ "jmp _after_" ++ l
    code s $ "_else_" ++ l ++ ":"
    emit s $ false
    code s $ "_after_" ++ l ++ ":"
emit s (Var depth) = do
    let reg = "[rbp-" ++ show (depth * 8) ++ "]"
    code s $ "mov rax, " ++ reg
emit s (Set depth e) = do
    let reg = "[rbp-" ++ show (depth * 8) ++ "]"
    emit s $ e
    code s $ "mov " ++ reg ++ ", rax"
emit s (Define depth e) = do
    let reg = "[rbp-" ++ show (depth * 8) ++ "]"
    emit s $ e
    code s $ "mov " ++ reg ++ ", rax"
emit s (While label cond es) = do
    let l = show label
    code s $ "_while_cond_" ++ l ++ ":"
    emit s $ cond
    code s $ "cmp rax, 0"
    code s $ "je _while_end_" ++ l
    mapM_ (emit s) es
    code s $ "jmp _while_cond_" ++ l
    code s $ "_while_end_" ++ l ++ ":"
emit s (Lambda label depth body) = do
    let reg = "[rbp-" ++ show (depth * 8) ++ "]"
    let l = "_lambda_" ++ show label
    -- TODO: For when I properly implement stack frames
    -- out l $ "push rbp"
    -- out l $ "mov rbp, rsp"
    -- out l $ "sub rsp, 100"
    code l $ "mov " ++ reg ++ ", rax"
    emit l $ body
    -- out l $ "mov rsp, rbp"
    -- out l $ "pop rbp"
    code l $ "ret"
    code s $ "lea rax, [" ++ l ++ "]"
emit s (Apply fun arg) = do
    emit s fun
    code s "push rax"
    emit s arg
    code s "pop rdx"
    code s "call rdx"
