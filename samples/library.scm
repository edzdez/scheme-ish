(define magnitude
  (lambda (x)
    (if (< x 0) (* x -1) x)))

(define remainder
  (lambda (x y)
    (- x (* y (/ x y)))))

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

