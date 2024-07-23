module ASM (makeASM) where

import IR as I

makeASM :: IR -> String
makeASM ir = prologue ++ emit ir ++ epilogue

prologue :: String
prologue =
    concat
        [ "global _main\n"
        , "extern _printf\n"
        , "extern _exit\n\n"
        , "section .data\n"
        , "default rel\n\n"
        , "format_int:\n"
        , "    db \"%d\", 10, 0\n\n"
        , "section .text\n\n"
        , "_main:\n"
        ]

epilogue :: String
epilogue =
    concat
        [ "    ; exit 0\n"
        , "    xor rdi, rdi\n"
        , "    call _exit"
        ]

emitBinOp :: IR -> IR -> String -> String -> String
emitBinOp left right comment op =
    concat
        [ "    ; " ++ comment ++ "\n"
        , emit right
        , "    push rax\n"
        , emit left
        , "    pop rdi\n"
        , "    " ++ op ++ "\n"
        ]

emit :: IR -> String
emit ir = case ir of
    Add e e' -> emitBinOp e e' "Addition" "add rax, rdi"
    Sub e e' -> emitBinOp e e' "Subtraction" "sub rax, rdi"
    Mul e e' -> emitBinOp e e' "Multiplication" "imul rax, rdi"
    Div e e' -> emitBinOp e e' "Division" "cqo\n    idiv rdi"
    Int n -> "    mov rax, " ++ show n ++ "\n"
    PrintInt e ->
        concat
            [ emit e
            , "    ; Print integer on rax\n"
            , "    lea rdi, [format_int]\n"
            , "    mov rsi, rax\n"
            , "    xor rax, rax\n"
            , "    call _printf\n"
            ]
    Begin es -> concat $ map emit es
    If l cond t f ->
        -- TODO: Implement Booleans instead of == 0
        concat
            [ "    ; If Statement\n"
            , emit cond
            , "    cmp rax, 0\n"
            , "    jne .else." ++ show l ++ "\n\n"
            , emit t
            , "    jmp .after." ++ show l ++ "\n"
            , ".else." ++ show l ++ ":\n"
            , emit f
            , ".after." ++ show l ++ ":\n"
            ]
