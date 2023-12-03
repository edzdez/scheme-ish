(load "samples/mergesort.scm")
(load "samples/pi.scm")

(define range
  (lambda (n)
    (if (equal? n 0)
        nil
        (cons (- n 1) (range (- n 1))))))

(define rev
  (lambda (l)
    (if (nil? l)
      l
      (append (rev (cdr l)) (list (car l))))))

(define make-random
  (lambda (n k)
    (if (equal? n 0)
      nil
      (cons (random (* -1 k) k)
            (make-random (- n 1) k)))))

(define random-10 (make-random 10 1000))
(define random-1000 (make-random 1000 1000))
(define range-10 (range 10))
(define range-1000 (range 100))
(define rev-range-10 (rev (range 10)))
(define rev-range-1000 (rev (range 100)))
(define pi-10 (take pi 10))
(define pi-1000 (take pi 1000))
