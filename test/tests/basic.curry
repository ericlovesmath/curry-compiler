(begin

    ; basic
    (put 1)
    (put (+ 12 -3))
    (put (- 12 -3))
    (put (* 12 -3))
    (put (/ 12 -3)) ; inline comment ignored
    (put (/ [+ 381 510] [* 3 {- 5 10}]))

    ; if
    (put (if #t 2 3)) ; 2
    (put (if #f 2 3)) ; 3
    (put (if (= 10 (+ 5 5)) 2 3)) ; 2
    (put (if (= 10 (- 5 5)) 2 3)) ; 3

    ; variables
    (begin
      (let x 4)
      (put x) ; 4
      (let y (* x 3))
      (put (+ x (* -1 y))) ; -8

      ; local scoping
      (begin
        (let x 5)
        (put x) ; 5
      )
      (put x) ; Still 4

      ; Modify from inner scope
      (begin
        (set x 10)
      )
      (put x) ; 10
    )

    ; while loops
    (begin
      (let n 10)
      (let a 1)
      (let b 1)
      (while n
         (put a)
         (let next (+ a b))
         (set a b)
         (set b next)
         (set n (- n 1))
      )
    )

    ; functions
    (begin
      (let double [lambda x (* x 2)])
      (let n 9)
      (let x 1)
      (while n
         (set x [double x])
         (set n (- n 1))
         (put x)
      )
    )

    ; function scope
    (begin
      (let f [lambda x (* x ([lambda x (+ x 2)] x))])
      (put (f 10)) ; 10 * (10 + 2)
    )

    ; currying and syntax sugar
    (begin
      (define f [x y z] [+ x (+ y z)])
      (let g (f 10)) ; g y z = 10 + y + z
      (put (g 6 7)) ; 23
    )

    ; function composition example
    (begin
      (define compose [f g x] [f (g x)])
      (define incr [x] (+ x 1))
      (define double [x] (* x 2))
      (let f (compose incr double))
      (put (f 5)) ; (5 * 2) + 1 = 11
    )
)
