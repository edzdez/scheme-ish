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

(define append
  (lambda (x y)
    (if (nil? x)
      y
      (cons (car x) (append (cdr x) y))))) 

(define take
  (lambda (l n)
    (if (< n 1)
      nil
      (cons (car l) (take (cdr l) (- n 1))))))

(define drop
  (lambda (l n)
    (if (< n 1)
      l
      (drop (cdr l) (- n 1)))))

(define slice
  (lambda (l start end)
      (take (drop l start) (- end start))))
