;; if statements are lazily evaluated
;; they only run the brach that they need to

;; for example, the false branch is not evaluated here
(define demo-if-true
  (lambda ()
    (if #t
        (display "hello from true")
        (display "hello from false"))))

;; for example, the true branch is not evaluated here
(define demo-if-false
  (lambda ()
    (if #f
        (display "hello from true")
        (display "hello from false"))))

;; lambdas are a sort of closure
;; they refer to the state of the program at the
;; time of creation

(define x 4)

;; at the time of creation
;; x is equal to 4
(define check
  (lambda ()
    (equal? x 4)))

;; thus, x is 4 in the check's closure
(define a (check))

;; in the global environment, x is now set to 5
(define x 5)

;; however, in the closure, x is still 4
(define b (check))

;; by renewing the closure, the state of the
;; program has x's value at 5
(define check
  (lambda ()
    (equal? x 4)))

;; and the check now fails
(define c (check))
