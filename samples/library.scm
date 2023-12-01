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

(define nil?
  (lambda (l)
    (equal? l nil)))

(define len
  (lambda (l)
    (if (nil? l)
        0
        (+ 1 (len (cdr l))))))


(define map
  (lambda (f l)
    (if (nil? l)
        nil
        (cons (f (car l)) (map f (cdr l))))))

(define filter
  (lambda (f l)
    (cond ((nil? l) nil)
          ((f (car l)) (cons (car l) (filter f (cdr l))))
          (else (filter f (cdr l))))))

(define reduce
  (lambda (f init l)
    (if (nil? l)
        init
        (f (car l) (reduce f init (cdr l))))))

(define list
  (lambda (. x)
    x))

