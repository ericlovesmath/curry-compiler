module ASM (makeASM) where

import Control.Monad.Writer
import IR

makeASM :: IR -> FilePath -> IO ()
makeASM ir filepath = do
    let ((), result) = runWriter emitter
    writeFile filepath result
  where
    emitter = do
        tell prologue
        emit ir
        tell epilogue

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
        , "_main:"
        ]

epilogue :: String
epilogue =
    unlines
        [ "; exit 0"
        , "xor rdi, rdi"
        , "call _exit"
        ]

emit :: IR -> Writer String ()
emit ir = case ir of
    Int n -> tell $ "mov rax, " ++ show n ++ "\n"
    Bool True -> emit $ Int 1
    Bool False -> emit $ Int 0
    Begin es -> mapM_ emit es
    BinaryOp op left right -> do
        emit right
        tell "push rax\n"
        emit left
        tell "pop rdi\n"
        tell $ case op of
            Add -> "add rax, rdi\n"
            Sub -> "sub rax, rdi\n"
            Mul -> "imul rax, rdi\n"
            Div -> unlines ["cqo", "idiv rdi"]
            Eq -> unlines ["cmp rax, rdi", "sete al", "movzx rax, al"]
    PrintInt e -> do
        emit e
        tell "lea rdi, [format_int]\n"
        tell "mov rsi, rax\n"
        tell "xor rax, rax\n"
        tell "call _printf\n"
    If label cond t f -> do
        emit cond
        tell "cmp rax, 0\n"
        let l = show label
        tell $ "je .else." ++ l ++ "\n"
        emit t
        tell $ "jmp .after." ++ l ++ "\n"
        tell $ ".else." ++ l ++ ":\n"
        emit f
        tell $ ".after." ++ l ++ ":\n"
    Var depth -> do
        let reg = "[rbp-" ++ show (depth * 8) ++ "]"
        tell $ "mov rax, " ++ reg ++ "\n"
    Set depth e -> do
        emit e
        let reg = "[rbp-" ++ show (depth * 8) ++ "]"
        tell $ "mov " ++ reg ++ ", rax\n"
    Define depth e -> do
        emit e
        let reg = "[rbp-" ++ show (depth * 8) ++ "]"
        tell $ "mov " ++ reg ++ ", rax\n"
