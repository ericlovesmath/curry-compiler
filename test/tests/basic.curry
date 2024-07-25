(begin

    ; basic
    (put 1)
    (put (+ 12 -3))
    (put (- 12 -3))
    (put (* 12 -3))
    (put (/ 12 -3)) ; inline comment ignored
    (put (/ (+ 381 510) (* 3 (- 5 10))))

    ; if
    (put (if #t 2 3)) ; 2
    (put (if #f 2 3)) ; 3
    (put (if (= 10 (+ 5 5)) 2 3)) ; 2
    (put (if (= 10 (- 5 5)) 2 3)) ; 3

    ; variables
    (begin
      (define x 4)
      (put x) ; 4
      (define y (* x 3))
      (put (+ x (* -1 y))) ; -8

      ; local scoping
      (begin
        (define x 5)
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
      (define n 10)
      (define a 1)
      (define b 1)
      (while n
         (put a)
         (define next (+ a b))
         (set a b)
         (set b next)
         (set n (- n 1))
      )
    )

)
