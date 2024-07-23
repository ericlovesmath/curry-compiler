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
        [ "    ; exit 0\n"
        , "    xor rdi, rdi\n"
        , "    call _exit"
        ]

emitBinOp :: A.Ast -> A.Ast -> String -> String -> String
emitBinOp left right comment op =
    concat
        [ "    ; " ++ comment ++ "\n"
        , emit right
        , "    push rax\n"
        , emit left
        , "    pop rdi\n"
        , "    " ++ op ++ "\n"
        ]

emit :: A.Ast -> String
emit ast = case ast of
    A.Add e e' -> emitBinOp e e' "Addition" "add rax, rdi"
    A.Sub e e' -> emitBinOp e e' "Subtraction" "sub rax, rdi"
    A.Mul e e' -> emitBinOp e e' "Multiplication" "imul rax, rdi"
    A.Div e e' -> emitBinOp e e' "Division" "cqo\n    idiv rdi"
    A.Int n -> "    mov rax, " ++ show n ++ "\n"
    A.PrintInt e ->
        concat
            [ emit e
            , "    ; Print integer on rax\n"
            , "    lea rdi, [format_int]\n"
            , "    mov rsi, rax\n"
            , "    xor rax, rax\n"
            , "    call _printf\n"
            ]

-- A.Begin exprs -> concat $ emit <$> exprs
