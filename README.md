# Curry

Compiler written Haskell for learning purposes. Functional by nature.

```scheme
; function composition example
(begin
  (define compose [f g x] [f (g x)])
  (define incr [x] (+ x 1))
  (define double [x] (* x 2))
  (let f (compose incr double))
  (put (f 5)) ; (5 * 2) + 1 = 11
)
```

## Current Tasks

1. Rename everything (including function bindings) a priori
2. Add IR to create label Mapping (Label maps to Function AND a map from their bound variables to their depth)

    - `Program :: [Map Label (IR, Map Variable Depth)]`
    - Create proper stack frames (Recursion support)
    - The current method of scoping does not work, and some things may break
    - Verify first-class functions work

3. Lambda Lifting (Closure support)
4. Compile to assembly intermediate instead of directly
5. Basic typechecking (up to higher-kinded types)

## Future Goals

- Typechecking
- Write out overall structure ^^;
- Refactor IR's `errors` to not be errors with Monad transformers
- Convert std functions to be curried by default
- Add more std functions (including non-binary operations)
- Plan out language features
- Non-Lispy syntax, since its not really a Lisp dialect
- Parsing to AST at the Parser Combinator level
- CLI to compile vs repl
- Document structure of compiler and examples of language
- Implement `printf` with static strings
- Write more tests for compiler's individual passes
- Dependent types
