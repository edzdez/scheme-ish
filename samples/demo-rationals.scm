(load "samples/library.scm")

(define make-rat
  (lambda (n d)
    (lambda (msg)
      (cond ((equal? msg "numer") n)
            ((equal? msg "denom") d)))))

(define add-rat
  (lambda (x y)
    (make-rat (+ (* (x "numer") (y "denom"))
                 (* (y "numer") (x "denom")))
              (* (x "denom") (y "denom")))))

(define sub-rat
  (lambda (x y)
    (make-rat (- (* (x "numer") (y "denom"))
                 (* (y "numer") (x "denom")))
              (* (x "denom") (y "denom")))))

(define mul-rat
  (lambda (x y)
    (make-rat (* (x "numer") (y "numer"))
              (* (x "denom") (y "denom")))))

(define div-rat
  (lambda (x y)
    (make-rat (* (x "numer") (y "denom"))
              (* (x "denom") (y "numer")))))

(define equal-rat?
  (lambda (x y)
    (equal? (* (x "numer") (y "denom"))
            (* (y "numer") (x "denom")))))
