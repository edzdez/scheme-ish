(define triangle-me-daddy
  (lambda (n)
    (if (equal? n 0)
        0
        (+ n (triangle-me-daddy (- n 1))))))

