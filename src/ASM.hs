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

emit :: IR -> String
emit ir = case ir of
    BinaryOp op left right ->
        concat
            [ "    ; " ++ comment ++ "\n"
            , emit right
            , "    push rax\n"
            , emit left
            , "    pop rdi\n"
            , "    " ++ asm ++ "\n"
            ]
      where
        (comment, asm) = case op of
            Add -> ("Addition", "add rax, rdi")
            Sub -> ("Subtraction", "sub rax, rdi")
            Mul -> ("Multiplication", "imul rax, rdi")
            Div -> ("Division", "cqo\n    idiv rdi")
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
    If label cond t f ->
        -- TODO: Implement Booleans instead of == 0
        concat
            [ "    ; If Statement\n"
            , emit cond
            , "    cmp rax, 0\n"
            , "    jne .else." ++ l ++ "\n\n"
            , emit t
            , "    jmp .after." ++ l ++ "\n"
            , ".else." ++ l ++ ":\n"
            , emit f
            , ".after." ++ l ++ ":\n"
            ]
        where
            l = show label
