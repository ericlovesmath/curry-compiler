(begin
    (printint 1)
    (printint (+ 12 -3))
    (printint (- 12 -3))
    (printint (* 12 -3))
    (printint (/ 12 -3))
    (printint (/ (+ 381 510) (* 3 (- 5 10))))
    (printint (if #t 2 3))
    (printint (if #f 2 3))
    (printint (if (= 10 (+ 5 5)) 2 3))
    (printint (if (= 10 (- 5 5)) 2 3))
)
