(define magnitude 
  (lambda (x)
    (if (< x 0) (* x -1) x)))

(define remainder
  (lambda (x y)
    (- x (* y (/ x y)))))

(define * 
  (lambda (x y)
    (cond ((< y 0) (- (* x (+ y 1)) x))
          ((> y 0) (+ x (* x (- y 1))))
          (else 0))))

(define /
  (lambda (x y)
    (define divide-abs
      (lambda (x y)
        (if (< x y)
          0
          (+ 1 (divide-abs (- x y) y)))))
    (define div (divide-abs (magnitude x) (magnitude y)))
    (if (< (* x y) 0)
      (* -1 div)
      div)))
