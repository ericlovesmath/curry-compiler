module ASM (compile) where

import AST as A

header :: String
header =
    concat
        [ "global _main\n"
        , "extern _printf\n"
        , "section .data\n"
        , "format_int:\n"
        , "    default rel\n"
        , "    db \"Int: %d\", 0\n"
        , "section .text\n\n"
        , "_main:\n"
        ]

footer :: String
footer =
    concat
        [ "    ; Print Int on Stack\n"
        , "    pop  rsi\n"
        , "    lea  rdi, [format_int]\n"
        , "    call _printf\n"
        , "    ret"
        ]

compile :: A.Ast -> String
compile ast = header ++ toAsm ast ++ footer

toAsm :: A.Ast -> String
toAsm ast = case ast of
    A.Add left right ->
        concat
            [ "    ; Addition\n"
            , toAsm left
            , toAsm right
            , "    pop rdi\n"
            , "    pop rax\n"
            , "    add rax, rdi\n"
            , "    push rax\n"
            ]
    A.Sub left right ->
        concat
            [ "    ; Subtraction\n"
            , toAsm left
            , toAsm right
            , "    pop rdi\n"
            , "    pop rax\n"
            , "    sub rax, rdi\n"
            , "    push rax\n"
            ]
    A.Int n ->
        concat
            [ "    ; Push integer\n"
            , "    mov rax, " ++ show n ++ "\n"
            , "    push rax\n"
            ]
