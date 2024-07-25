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
    let codeMap = execState emitter Map.empty
    let output [] = ""
        output (m : ms) = fst m ++ ":\n" ++ snd m ++ "\n" ++ output ms
    let res = output (Map.toList codeMap)
    writeFile filepath (prologue ++ res)
  where
    emitter = do
        emit "_main" ir
        out "_main" epilogue

prologue :: String
prologue =
    unlines
        [ "global _main"
        , "extern _printf"
        , "extern _exit\n"
        , "section .data"
        , "default rel"
        , "format_int:"
        , "    db \"%d\", 10, 0\n"
        , "section .text\n"
        ]

epilogue :: String
epilogue = unlines ["xor rdi, rdi", "call _exit"]

out :: Label -> String -> Writer ()
out label line = do
    m <- get
    put $ Map.insertWith (flip (++)) label line m

emit :: Label -> IR -> Writer ()
emit s ir = case ir of
    Int n -> out s $ "mov rax, " ++ show n ++ "\n"
    Bool True -> emit s $ Int 1
    Bool False -> emit s $ Int 0
    Begin es -> mapM_ (emit s) es
    BinaryOp op left right -> do
        emit s right
        out s "push rax\n"
        emit s left
        out s "pop rdi\n"
        out s $ case op of
            Add -> "add rax, rdi\n"
            Sub -> "sub rax, rdi\n"
            Mul -> "imul rax, rdi\n"
            Div -> unlines ["cqo", "idiv rdi"]
            Eq -> unlines ["cmp rax, rdi", "sete al", "movzx rax, al"]
    PrintInt e -> do
        emit s e
        out s "lea rdi, [format_int]\n"
        out s "mov rsi, rax\n"
        out s "xor rax, rax\n"
        out s "call _printf\n"
    If label cond t f -> do
        emit s cond
        out s "cmp rax, 0\n"
        let l = show label
        out s $ "je _else." ++ l ++ "\n"
        emit s t
        out s $ "jmp _after." ++ l ++ "\n"
        out s $ "_else." ++ l ++ ":\n"
        emit s f
        out s $ "_after." ++ l ++ ":\n"
    Var depth -> do
        let reg = "[rbp-" ++ show (depth * 8) ++ "]"
        out s $ "mov rax, " ++ reg ++ "\n"
    Set depth e -> do
        emit s e
        let reg = "[rbp-" ++ show (depth * 8) ++ "]"
        out s $ "mov " ++ reg ++ ", rax\n"
    Define depth e -> do
        emit s e
        let reg = "[rbp-" ++ show (depth * 8) ++ "]"
        out s $ "mov " ++ reg ++ ", rax\n"
    While label cond es -> do
        let l = show label
        out s $ ".whilecond." ++ l ++ ":\n"
        emit s cond
        out s "cmp rax, 0\n"
        out s $ "je .whileend." ++ l ++ "\n"
        mapM_ (emit s) es
        out s $ "jmp .whilecond." ++ l ++ "\n"
        out s $ ".whileend." ++ l ++ ":\n"
    Lambda label depth body -> do
        let reg = "[rbp-" ++ show (depth * 8) ++ "]"
        let l = "_lambda." ++ show label
        -- out l $ "push rbp\n"
        -- out l $ "mov rbp, rsp\n"
        -- out l $ "sub rsp, 100\n"
        out l $ "mov " ++ reg ++ ", rax\n"
        emit l body
        -- out l $ "mov rsp, rbp\n"
        -- out l $ "pop rbp\n"
        out l $ "ret\n"
        out s $ "lea rax, [" ++ l ++ "]\n"
    Apply label fun arg -> do
        emit s fun
        out s "push rax\n"
        emit s arg
        out s "pop rdx\n"
        out s $ "call rdx\n"
        -- out s $ "call _lambda." ++ show label ++ "\n"
