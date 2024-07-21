module ASM (asm) where

import AST as A

-- TODO: All of this header and footer is basically temporary

asm :: A.Ast -> String
asm ast = header ++ emit ast ++ footer

header :: String
header =
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

footer :: String
footer =
    concat
        [ "    ; Print integer on stack\n"
        , "    pop rsi\n"
        , "    lea rdi, [format_int]\n"
        , "    xor rax, rax\n"
        , "    call _printf\n\n"
        , "    ; exit 0\n"
        , "    xor rdi, rdi\n"
        , "    call _exit"
        ]

emit :: A.Ast -> String
emit ast = case ast of
    A.Add left right ->
        concat
            [ "    ; Addition\n"
            , emit left
            , emit right
            , "    pop rdi\n"
            , "    pop rax\n"
            , "    add rax, rdi\n"
            , "    push rax\n\n"
            ]
    A.Sub left right ->
        concat
            [ "    ; Subtraction\n"
            , emit left
            , emit right
            , "    pop rdi\n"
            , "    pop rax\n"
            , "    sub rax, rdi\n"
            , "    push rax\n\n"
            ]
    A.Mul left right ->
        concat
            [ "    ; Multiplication\n"
            , emit left
            , emit right
            , "    pop rdi\n"
            , "    pop rax\n"
            , "    imul rax, rdi\n"
            , "    push rax\n\n"
            ]
    A.Div left right ->
        concat
            [ "    ; Division\n"
            , emit left
            , emit right
            , "    pop rdi\n"
            , "    pop rax\n"
            , "    cqo\n"
            , "    idiv rdi\n"
            , "    push rax\n\n"
            ]
    A.Int n -> "    push " ++ show n ++ "\n"
